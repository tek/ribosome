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
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer(MenuConsumer))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QuitReason)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
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
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

promptEvent ::
  PromptEvent ->
  Prompt ->
  PromptConsumed ->
  MenuEvent m a
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
  Monad m =>
  MenuConsumer m a ->
  Either PromptConsumerUpdate MenuItem ->
  ConduitT (Either PromptConsumerUpdate MenuItem) (MenuUpdate m a) (StateT Menu m) ()
updateMenu (MenuConsumer consumer) input =
  emit =<< (lift . stateM $ lift . consumer . MenuUpdate (menuEvent input))
  where
    emit MenuConsumerAction.Continue =
      update (menuEvent input)
    emit (MenuConsumerAction.QuitWith ma) =
      update (MenuEvent.Quit (QuitReason.Execute ma))
    emit MenuConsumerAction.Quit =
      update (MenuEvent.Quit QuitReason.Aborted)
    emit (MenuConsumerAction.Return a) =
      update (MenuEvent.Quit (QuitReason.Return a))
    update event =
      yield . MenuUpdate event =<< get

menuSources ::
  MonadIO m =>
  PromptConfig m ->
  ConduitT () MenuItem m () ->
  [ConduitT () (Either PromptConsumerUpdate MenuItem) m ()]
menuSources promptConfig items =
  [promptSource, itemSource]
  where
    promptSource =
      promptC promptConfig .| mapC Left
    itemSource =
      items .| mapC Right

menuTerminator ::
  Monad m =>
  ConduitT (MenuUpdate m a) (QuitReason m a) m ()
menuTerminator =
  traverse_ check =<< await
  where
    check (MenuUpdate (MenuEvent.Quit reason) _) =
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
  MonadBaseControl IO m =>
  MenuConfig m a ->
  m (MenuResult a)
runMenu (MenuConfig items handle render promptConfig@(PromptConfig _ _ promptRenderer _)) =
  withPrompt promptRenderer
  where
    withPrompt (PromptRenderer acquire release renderPrompt) =
      bracket acquire release (const run)
    run =
      menuResult =<< quitReason <$> withMergedSources 64 consumer (menuSources promptConfig items)
    consumer =
      evalStateC def (awaitForever (updateMenu handle)) .| iterM render .| menuTerminator .| Conduit.last
    quitReason =
      fromMaybe QuitReason.NoOutput

nvimMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  ConduitT () MenuItem m () ->
  (MenuUpdate m a -> m (MenuConsumerAction m a, Menu)) ->
  PromptConfig m ->
  m (MenuResult a)
nvimMenu options items handle promptConfig =
  run =<< showInScratch [] options
  where
    run scratch =
      runMenu $ MenuConfig items (MenuConsumer handle) (render scratch) promptConfig
    render =
      renderNvimMenu options
