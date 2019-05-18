module Ribosome.Menu.Run where

import Conduit (ConduitT, await, awaitForever, mapC, yield, (.|))
import Control.Monad.Trans.Control (MonadBaseControl)
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
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason(NoOutput, PromptError, Regular))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Nvim (renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Run (promptC)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

promptEvent ::
  PromptEvent ->
  Prompt ->
  MenuEvent
promptEvent (PromptEvent.Character a) prompt@(Prompt _ PromptState.Insert _) =
  MenuEvent.PromptChange a prompt
promptEvent (PromptEvent.Character a) prompt =
  MenuEvent.Mapping a prompt
promptEvent PromptEvent.Init prompt =
  MenuEvent.Init prompt
promptEvent (PromptEvent.Unexpected code) _ =
  MenuEvent.Quit . QuitReason.PromptError $ "unexpected input character code: " <> show code

menuUpdate ::
  Either PromptConsumerUpdate MenuItem ->
  Menu ->
  MenuUpdate
menuUpdate input =
  MenuUpdate $ either promptUpdate MenuEvent.NewItems input
  where
    promptUpdate (PromptConsumerUpdate event prompt) =
      promptEvent event prompt

updateMenu ::
  Monad m =>
  MenuConsumer m ->
  Either PromptConsumerUpdate MenuItem ->
  ConduitT (Either PromptConsumerUpdate MenuItem) MenuUpdate (StateT Menu m) ()
updateMenu (MenuConsumer consumer) input =
  emit =<< (lift . stateM $ lift . consumer . menuUpdate input)
  where
    emit MenuConsumerAction.Continue =
      yield =<< menuUpdate input <$> get
    emit _ =
      yield . MenuUpdate (MenuEvent.Quit QuitReason.Regular) =<< get

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
  ConduitT MenuUpdate QuitReason m ()
menuTerminator =
  traverse_ check =<< await
  where
    check (MenuUpdate (MenuEvent.Quit reason) _) =
      yield reason
    check _ =
      menuTerminator

runMenu ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuConfig m ->
  m QuitReason
runMenu (MenuConfig items handle render promptConfig) =
  quitReason <$> withMergedSources 64 consumer (menuSources promptConfig items)
  where
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
  (MenuUpdate -> m (MenuConsumerAction m, Menu)) ->
  PromptConfig m ->
  m QuitReason
nvimMenu options items handle promptConfig =
  run =<< showInScratch [] options
  where
    run scratch =
      runMenu $ MenuConfig items (MenuConsumer handle) (render scratch) promptConfig
    render =
      renderNvimMenu options
