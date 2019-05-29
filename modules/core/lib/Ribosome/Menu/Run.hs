module Ribosome.Menu.Run where

import Conduit (ConduitT, await, awaitForever, mapC, yield, (.|))
import Control.Exception.Lifted (bracket, catch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Composition ((.::))
import Data.Conduit.Combinators (iterM)
import qualified Data.Conduit.Combinators as Conduit (last)
import Data.Conduit.Lift (evalStateC)

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Conduit (withMergedSources)
import Ribosome.Data.Scratch (scratchWindow)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Log (showDebug)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction(..))
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer(MenuConsumer))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QuitReason)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent(..))
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Nvim (renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed)
import qualified Ribosome.Menu.Prompt.Data.PromptConsumed as PromptConsumed (PromptConsumed(..))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer(PromptRenderer))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Nvim (promptBlocker)
import Ribosome.Menu.Prompt.Run (promptC)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (windowSetOption)
import Ribosome.Scratch (showInScratch)

promptEvent ::
  PromptEvent ->
  Prompt ->
  PromptConsumed ->
  MenuEvent m a
promptEvent _ (Prompt _ PromptState.Quit _) _ =
  MenuEvent.Quit QuitReason.Aborted
promptEvent (PromptEvent.Character a) prompt PromptConsumed.No =
  MenuEvent.Mapping a prompt
promptEvent (PromptEvent.Character a) prompt@(Prompt _ PromptState.Insert _) _ =
  MenuEvent.PromptChange a prompt
promptEvent (PromptEvent.Character a) prompt _ =
  MenuEvent.Mapping a prompt
promptEvent PromptEvent.Init prompt _ =
  MenuEvent.Init prompt
promptEvent (PromptEvent.Unexpected code) _ _ =
  MenuEvent.Quit . QuitReason.PromptError $ "unexpected input character code: " <> show code
promptEvent PromptEvent.Interrupt _ _ =
  MenuEvent.Quit QuitReason.Aborted
promptEvent (PromptEvent.Error e) _ _ =
  MenuEvent.Quit (QuitReason.PromptError e)

menuEvent ::
  Either PromptConsumerUpdate MenuItem ->
  MenuEvent m a
menuEvent =
  either promptUpdate MenuEvent.NewItems
  where
    promptUpdate (PromptConsumerUpdate event prompt consumed) =
      promptEvent event prompt consumed

updateMenu ::
  MonadRibo m =>
  MenuConsumer m a ->
  Either PromptConsumerUpdate MenuItem ->
  ConduitT (Either PromptConsumerUpdate MenuItem) (MenuRenderEvent m a) (StateT Menu m) ()
updateMenu (MenuConsumer consumer) input = do
  showDebug "menu update:" input
  action <- lift . stateM $ lift . consumer . MenuUpdate (menuEvent input)
  showDebug "menu action:" action
  emit action
  where
    emit MenuAction.Continue =
      return ()
    emit (MenuAction.Render changed) =
      yield . MenuRenderEvent.Render changed =<< get
    emit (MenuAction.Quit reason) =
      yield (MenuRenderEvent.Quit reason)

menuSources ::
  MonadIO m =>
  MonadRibo m =>
  PromptConfig m ->
  ConduitT () MenuItem m () ->
  [CConduit () (Either PromptConsumerUpdate MenuItem) m ()]
menuSources promptConfig items =
  [promptSource, itemSource]
  where
    promptSource =
      ccMap (.| mapC Left) (promptC promptConfig)
    itemSource =
      CConduit.Single $ items .| mapC Right

menuTerminator ::
  Monad m =>
  ConduitT (MenuRenderEvent m a) (QuitReason m a) m ()
menuTerminator =
  traverse_ check =<< await
  where
    check (MenuRenderEvent.Quit reason) =
      yield reason
    check _ =
      menuTerminator

menuResult ::
  Monad m =>
  QuitReason m a ->
  m (MenuResult a)
menuResult (QuitReason.Return a) =
  return (MenuResult.Return a)
menuResult (QuitReason.Execute ma) =
  MenuResult.Return <$> ma
menuResult (QuitReason.PromptError err) =
  return (MenuResult.Error err)
menuResult QuitReason.NoOutput =
  return MenuResult.NoOutput
menuResult QuitReason.Aborted =
  return MenuResult.Aborted

runMenu ::
  MonadIO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MenuConfig m a ->
  m (MenuResult a)
runMenu (MenuConfig items handle render promptConfig@(PromptConfig _ _ promptRenderer _)) =
  withPrompt promptRenderer
  where
    withPrompt (PromptRenderer acquire release renderPrompt) =
      bracket acquire release (const run)
    run =
      menuResult =<< quitReason <$> withMergedSources consumer 64 (menuSources promptConfig items)
    consumer =
      evalStateC def (awaitForever (updateMenu handle)) .| iterM render .| menuTerminator .| Conduit.last
    quitReason =
      fromMaybe QuitReason.NoOutput

nvimMenu ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  ConduitT () MenuItem m () ->
  (MenuUpdate m a -> m (MenuAction m a, Menu)) ->
  PromptConfig m ->
  m (MenuResult a)
nvimMenu options items handle promptConfig =
  run =<< showInScratch [] options
  where
    run scratch = do
      windowSetOption (scratchWindow scratch) "cursorline" (toMsgpack True)
      runMenu $ MenuConfig items (MenuConsumer handle) (render scratch) promptConfig
    render =
      renderNvimMenu options
