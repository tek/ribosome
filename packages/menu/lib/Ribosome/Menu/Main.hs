module Ribosome.Menu.Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Exon (exon)
import Polysemy.Conc (interpretAtomic, interpretSync, interpretSyncAs)
import qualified Polysemy.Log as Log
import Prelude hiding (consume)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, SerialT)

import Ribosome.Final (inFinal_)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction (..))
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (
  CursorLock (CursorLock),
  ItemsLock (ItemsLock),
  MenuStack,
  MenuStateEffects,
  readMenu,
  )
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer, menuConsumerEvent)
import qualified Ribosome.Menu.Effect.MenuRenderer as MenuRenderer
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptListening, startInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Run (pristinePrompt, withPromptStream)
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

renderAction ::
  Members [MenuRenderer i, Log] r =>
  Members (MenuStateEffects i) r =>
  MenuAction a ->
  Sem r ()
renderAction = \case
  MenuAction.Render ->
    MenuRenderer.menuRender =<< readMenu
  MenuAction.Quit _ ->
    MenuRenderer.menuRenderQuit
  _ ->
    unit

menuStream ::
  ∀ i r a .
  Show a =>
  Members (MenuStack i) r =>
  Members [MenuRenderer i, MenuConsumer a, Resource, Log, Embed IO, Final IO] r =>
  MenuConfig i a ->
  TMChan PromptControlEvent ->
  AsyncT IO (Prompt, PromptEvent) ->
  Sem r (SerialT IO (MenuResult a))
menuStream (MenuConfig items itemFilter _) promptControl promptEvents =
  inFinal_ \ lowerMaybe lower_ pur ->
  let
    handleAction action =
      lower_ do
        Log.debug [exon|menu consumer: #{show action}|]
        renderAction action
    prompt =
      promptEvent lowerMaybe itemFilter promptEvents
    menuItems =
      updateItems lowerMaybe itemFilter (Stream.fromSerial items)
    consume event = do
      menuAction <- lowerMaybe (menuConsumerEvent event)
      pure (fromMaybe (eventAction event) (join menuAction))
    quit = do
      atomically (writeTMChan promptControl PromptControlEvent.Quit)
      lower_ MenuRenderer.menuRenderQuit
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
    pure MenuResult.NoAction
  where
    describe = \case
      MenuResult.Success _ -> "success"
      MenuResult.Error msg -> msg
      MenuResult.Aborted -> "user interrupt"
      MenuResult.NoAction -> "no action"

interpretMenu ::
  ∀ i r .
  Members [Resource, Race, Embed IO] r =>
  InterpretersFor (Sync PromptListening : MenuStack i) r
interpretMenu =
  interpretSyncAs CursorLock .
  interpretSyncAs ItemsLock .
  subsume .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretSync @PromptListening

menuMain ::
  Show a =>
  Members (MenuStack i) r =>
  Members [PromptEvents, PromptRenderer] r =>
  Members [MenuConsumer a, MenuRenderer i, Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  MenuConfig i a ->
  Sem r (MenuResult a)
menuMain conf =
  withPromptStream initialPrompt promptInput \ (promptControl, promptEvents) -> do
    stream <- menuStream conf promptControl (Stream.fromSerial promptEvents)
    menuResult =<< embed (Stream.last stream)
  where
    initialPrompt =
      pristinePrompt (startInsert flags)
    PromptConfig promptInput flags =
      conf ^. #prompt

simpleMenu ::
  Show a =>
  Member (MenuRenderer i) r =>
  Members [PromptEvents, Scoped pres PromptRenderer, Log, Mask mres, Resource, Race, Embed IO, Final IO] r =>
  InterpreterFor (MenuConsumer a) (Sync PromptListening : MenuStack i ++ PromptRenderer : r) ->
  MenuConfig i a ->
  Sem r (MenuResult a)
simpleMenu consumer config =
  withPrompt do
    interpretMenu $ consumer $ menuMain config

menu ::
  ∀ a i pres mrres res r .
  Show a =>
  Members (MenuStack i) r =>
  Members [PromptEvents, MenuConsumer a, Scoped pres PromptRenderer] r =>
  Members [Scoped mrres (MenuRenderer i), Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  MenuConfig i a ->
  Sem r (MenuResult a)
menu conf =
  withMenuRenderer $ withPrompt do
    menuMain conf
