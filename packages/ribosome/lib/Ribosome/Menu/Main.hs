module Ribosome.Menu.Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Prelude hiding (consume)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, IsStream)

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
import Ribosome.Menu.Data.MenuState (MenuState, newMenuState, readMenuForRender)
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Run (promptStream)
import Ribosome.Menu.UpdateState (promptEvent, updateItems)

eventAction ::
  Applicative m =>
  MenuEvent ->
  MenuAction m a
eventAction = \case
  MenuEvent.Init ->
    MenuAction.Continue
  MenuEvent.PromptEdit ->
    MenuAction.Render
  MenuEvent.PromptNavigation ->
    MenuAction.Render
  MenuEvent.Mapping _ ->
    MenuAction.Continue
  MenuEvent.NewItem ->
    MenuAction.Render
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
  MenuAction m a ->
  m (Maybe (m (MenuResult a)))
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

-- TODO Log
runRenderer ::
  MonadIO m =>
  MenuState i ->
  MenuRenderer m i ->
  MenuRenderEvent ->
  m ()
runRenderer menu (MenuRenderer render) event = do
  -- Log.debug [exon|render: #{show event}|]
  flip render event =<< readMenuForRender menu

renderAction ::
  MonadIO m =>
  MenuState i ->
  MenuRenderer m i ->
  MenuAction m a ->
  m ()
renderAction menu (runRenderer menu -> run) = \case
  MenuAction.Render ->
    run MenuRenderEvent.Render
  MenuAction.Quit _ ->
    run MenuRenderEvent.Quit
  _ ->
    unit

-- TODO Log
menuStream ::
  MonadIO m =>
  IsStream t =>
  MonadCatch m =>
  Functor (t m) =>
  MonadBaseControl IO m =>
  MenuState i ->
  MenuConfig m i a ->
  TMChan PromptControlEvent ->
  AsyncT m (Prompt, PromptEvent) ->
  t m (m (MenuResult a))
menuStream menu (MenuConfig items itemFilter (MenuConsumer consumer) renderer _) promptControl promptEvents =
  Stream.finally quit $
  Stream.mapMaybeM (outputAction promptControl) $
  Stream.tapAsync (Fold.drainBy handleAction) $
  Stream.mapM consume $
  Stream.parallelFst prompt menuItems
  where
    handleAction action = do
      -- Log.debug [exon|menu consumer: #{show action}|]
      renderAction menu renderer action
    prompt =
      promptEvent menu itemFilter promptEvents
    menuItems =
      Stream.fromSerial (updateItems menu itemFilter items)
    consume event =
      fromMaybe (eventAction event) <$> consumer menu event
    quit = do
      liftIO (atomically (writeTMChan promptControl PromptControlEvent.Quit))
      runRenderer menu renderer MenuRenderEvent.Quit

-- TODO Log
menuResult ::
  Monad m =>
  Maybe (m (MenuResult a)) ->
  m (MenuResult a)
menuResult = \case
  Just resultAction -> do
    result <- resultAction
    pure result
    -- result <$ Log.debug [exon|menu terminated: #{describe result}|]
  Nothing -> do
    -- Log.debug "menu terminated without output"
    pure (MenuResult.Error "no output")
  where
    -- describe = \case
    --   MenuResult.Success _ -> "success"
    --   MenuResult.Error msg -> msg
    --   MenuResult.Aborted -> "user interrupt"

menuMain ::
  MonadIO m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MenuConfig m i a ->
  m (MenuResult a)
menuMain conf = do
  menu <- newMenuState
  (promptControl, promptEvents) <- promptStream (conf ^. MenuConfig.prompt)
  menuResult =<< Stream.last (menuStream menu conf promptControl (Stream.fromSerial promptEvents))
