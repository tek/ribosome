module Ribosome.Menu.Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import Control.Lens ((^.))
import Exon (exon)
import Polysemy.Conc (interpretAtomic, interpretSyncAs)
import qualified Polysemy.Log as Log
import Prelude hiding (consume)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, SerialT)

import Ribosome.Final (inFinal_)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction (..))
import qualified Ribosome.Menu.Data.MenuConfig as MenuConfig
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer (MenuConsumer))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent (..))
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer (MenuRenderer))
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (
  CursorLock (CursorLock),
  ItemsLock (ItemsLock),
  MenuStateSem,
  MenuStateStack,
  readMenu,
  )
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptListening, hoistPromptConfig)
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Run (withPromptStream)
import Ribosome.Menu.UpdateState (promptEvent, updateItems)

eventAction ::
  MenuEvent ->
  MenuAction r a
eventAction = \case
  MenuEvent.Init ->
    MenuAction.Continue
  MenuEvent.PromptEdit ->
    MenuAction.Render
  MenuEvent.PromptNavigation ->
    MenuAction.Render
  MenuEvent.Mapping _ ->
    MenuAction.Continue
  MenuEvent.NewItems ->
    MenuAction.Render
  MenuEvent.Exhausted ->
    MenuAction.Continue
  MenuEvent.Quit QuitReason.Aborted ->
    MenuAction.Quit (pure MenuResult.Aborted)
  MenuEvent.Quit (QuitReason.Error msg) ->
    MenuAction.Quit (pure (MenuResult.Error msg))

toPrompt ::
  MonadIO m =>
  TMChan PromptControlEvent ->
  PromptControlEvent ->
  m ()
toPrompt promptControl =
  liftIO . atomically . writeTMChan promptControl

quitPrompt ::
  MonadIO m =>
  TMChan PromptControlEvent ->
  m ()
quitPrompt promptControl =
  toPrompt promptControl PromptControlEvent.Quit

outputAction ::
  MonadIO m =>
  TMChan PromptControlEvent ->
  MenuAction r a ->
  m (Maybe (Sem r (MenuResult a)))
outputAction promptControl = \case
  MenuAction.Continue ->
    pure Nothing
  MenuAction.Render ->
    pure Nothing
  MenuAction.UpdatePrompt prompt ->
    Nothing <$ sendPromptEvent (PromptControlEvent.Set prompt)
  MenuAction.Quit result ->
    Just result <$ quitPrompt promptControl
  where
    sendPromptEvent =
      toPrompt promptControl

runRenderer ::
  Member Log r =>
  MenuRenderer r i ->
  MenuRenderEvent ->
  MenuStateSem i r ()
runRenderer (MenuRenderer render) event = do
  Log.debug [exon|render: #{show event}|]
  insertAt @0 . flip render event =<< readMenu

renderAction ::
  Member Log r =>
  MenuRenderer r i ->
  MenuAction r a ->
  MenuStateSem i r ()
renderAction (runRenderer -> run) = \case
  MenuAction.Render ->
    run MenuRenderEvent.Render
  MenuAction.Quit _ ->
    run MenuRenderEvent.Quit
  _ ->
    unit

menuStream ::
  ∀ i r a .
  Members [Resource, Log, Embed IO, Final IO] r =>
  MenuConfig r i a ->
  TMChan PromptControlEvent ->
  AsyncT IO (Prompt, PromptEvent) ->
  MenuStateSem i r (SerialT IO (Sem r (MenuResult a)))
menuStream (MenuConfig items itemFilter (MenuConsumer consumer) renderer _) promptControl promptEvents =
  inFinal_ \ lowerMaybe lower_ pur ->
  let
    handleAction action = do
      lower_ do
        Log.debug [exon|menu consumer: #{show action}|]
        insertAt @0 (renderAction renderer action)
    prompt =
      promptEvent lowerMaybe itemFilter promptEvents
    menuItems =
      updateItems lowerMaybe itemFilter (Stream.fromSerial items)
    consume event = do
      menuAction <- lowerMaybe (consumer event)
      pure (fromMaybe (eventAction event) (join menuAction))
    quit = do
      atomically (writeTMChan promptControl PromptControlEvent.Quit)
      lower_ (insertAt @0 (runRenderer renderer MenuRenderEvent.Quit))
    in
      pur $
      Stream.finally quit $
      Stream.mapMaybeM (outputAction promptControl) $
      Stream.tapAsync (Fold.drainBy handleAction) $
      Stream.mapM consume $
      Stream.parallelFst prompt menuItems

menuResult ::
  Members [Log, Embed IO] r =>
  Maybe (Sem r (MenuResult a)) ->
  Sem r (MenuResult a)
menuResult = \case
  Just resultAction -> do
    result <- resultAction
    result <$ Log.debug [exon|menu terminated: #{describe result}|]
  Nothing -> do
    Log.debug "menu terminated without output"
    pure (MenuResult.Error "no output")
  where
    describe = \case
      MenuResult.Success _ -> "success"
      MenuResult.Error msg -> msg
      MenuResult.Aborted -> "user interrupt"

interpretMenu ::
  Members [Race, Embed IO] r =>
  InterpretersFor (MenuStateStack i) r
interpretMenu =
  interpretSyncAs CursorLock .
  interpretSyncAs ItemsLock .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def

menuMain ::
  Members [Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  MenuConfig r i a ->
  Sem r (MenuResult a)
menuMain conf =
  interpretMenu $
  withPromptStream (hoistPromptConfig (insertAt @0) (conf ^. MenuConfig.prompt)) \ (promptControl, promptEvents) -> do
    stream <- menuStream conf promptControl (Stream.fromSerial promptEvents)
    insertAt @0 . menuResult =<< embed (Stream.last stream)
