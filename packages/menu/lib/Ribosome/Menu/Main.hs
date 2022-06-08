module Ribosome.Menu.Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import Control.Lens ((^.))
import Data.Generics.Labels ()
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
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig), hoistMenuConfig)
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
  MenuStateEffects,
  MenuStateStack,
  readMenu,
  )
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer, menuConsumerEvent)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptListening, hoistPromptConfig)
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Run (withPromptStream)
import Ribosome.Menu.UpdateState (promptEvent, updateItems)

eventAction ::
  MenuEvent ->
  MenuAction a
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
    MenuAction.Quit MenuResult.Aborted
  MenuEvent.Quit (QuitReason.Error msg) ->
    MenuAction.Quit (MenuResult.Error msg)

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
  MenuAction a ->
  m (Maybe (MenuResult a))
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
  Members (MenuStateEffects i) r =>
  MenuRenderer r i ->
  MenuRenderEvent ->
  Sem r ()
runRenderer (MenuRenderer render) event = do
  Log.debug [exon|render: #{show event}|]
  insertAt @0 . flip render event =<< readMenu

renderAction ::
  Member Log r =>
  Members (MenuStateEffects i) r =>
  MenuRenderer r i ->
  MenuAction a ->
  Sem r ()
renderAction (runRenderer -> run) = \case
  MenuAction.Render ->
    run MenuRenderEvent.Render
  MenuAction.Quit _ ->
    run MenuRenderEvent.Quit
  _ ->
    unit

menuStream ::
  ∀ i r a .
  Show a =>
  Members (MenuStateStack i) r =>
  Members [MenuConsumer a, Resource, Log, Embed IO, Final IO] r =>
  MenuConfig r i a ->
  TMChan PromptControlEvent ->
  AsyncT IO (Prompt, PromptEvent) ->
  Sem r (SerialT IO (MenuResult a))
menuStream (MenuConfig items itemFilter renderer _) promptControl promptEvents =
  inFinal_ \ lowerMaybe lower_ pur ->
  let
    handleAction action =
      lower_ do
        Log.debug [exon|menu consumer: #{show action}|]
        insertAt @0 (renderAction renderer action)
    prompt =
      promptEvent lowerMaybe itemFilter promptEvents
    menuItems =
      updateItems lowerMaybe itemFilter (Stream.fromSerial items)
    consume event = do
      menuAction <- lowerMaybe (menuConsumerEvent event)
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
  Maybe (MenuResult a) ->
  Sem r (MenuResult a)
menuResult = \case
  Just result ->
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
  Show a =>
  Members (MenuStateStack i) r =>
  Members [MenuConsumer a, Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  MenuConfig r i a ->
  Sem r (MenuResult a)
menuMain conf =
  withPromptStream (hoistPromptConfig (insertAt @0) (conf ^. #prompt)) \ (promptControl, promptEvents) -> do
    stream <- menuStream conf promptControl (Stream.fromSerial promptEvents)
    menuResult =<< embed (Stream.last stream)

runMenuMain ::
  Show a =>
  Members [Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ r) ->
  MenuConfig r i a ->
  Sem r (MenuResult a)
runMenuMain consumer config =
  interpretMenu $
  consumer $
  menuMain (hoistMenuConfig (insertAt @0) config)

runMenu ::
  Show a =>
  Members [Scoped pres PromptRenderer, Sync PromptListening, Log, Mask mres, Resource, Race, Embed IO, Final IO] r =>
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ PromptRenderer : r) ->
  MenuConfig r i a ->
  Sem r (MenuResult a)
runMenu consumer config =
  withPrompt do
    runMenuMain consumer (hoistMenuConfig raise config)
