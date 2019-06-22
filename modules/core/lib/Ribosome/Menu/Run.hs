module Ribosome.Menu.Run where

import Conduit (ConduitT, await, awaitForever, mapC, yield, (.|))
import Control.Exception.Lifted (bracket)
import Control.Lens (over, set)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit.Combinators (iterM)
import qualified Data.Conduit.Combinators as Conduit (last)
import Data.Conduit.Lift (evalStateC)

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Conduit (withMergedSources)
import Ribosome.Data.Scratch (scratchWindow)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (size)
import Ribosome.Log (showDebug)
import Ribosome.Menu.Data.Menu (Menu)
import qualified Ribosome.Menu.Data.Menu as Menu (maxItems)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction(..))
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer(MenuConsumer))
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QuitReason)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
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
import Ribosome.Menu.Prompt.Run (promptC)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (windowSetOption)
import Ribosome.Scratch (showInScratch)

promptEvent ::
  PromptEvent ->
  Prompt ->
  PromptConsumed ->
  MenuEvent m a i
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
  Either PromptConsumerUpdate [MenuItem i] ->
  MenuEvent m a i
menuEvent =
  either promptUpdate MenuEvent.NewItems
  where
    promptUpdate (PromptConsumerUpdate event prompt consumed) =
      promptEvent event prompt consumed

updateMenu ::
  MonadRibo m =>
  MenuConsumer m a i ->
  Either PromptConsumerUpdate [MenuItem i] ->
  ConduitT (Either PromptConsumerUpdate [MenuItem i]) (MenuRenderEvent m a i) (StateT (Menu i) m) ()
updateMenu (MenuConsumer consumer) input = do
  showDebug "menu update:" (MenuItem._text <$$> input)
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
  ConduitT () [MenuItem i] m () ->
  [ConduitT () (Either PromptConsumerUpdate [MenuItem i]) m ()]
menuSources promptConfig items =
  [promptSource, itemSource]
  where
    promptSource =
      promptC promptConfig .| mapC Left
    itemSource =
      items .| mapC Right

menuTerminator ::
  Monad m =>
  ConduitT (MenuRenderEvent m a i) (QuitReason m a) m ()
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
  MenuConfig m a i ->
  m (MenuResult a)
runMenu (MenuConfig items handle render promptConfig@(PromptConfig _ _ promptRenderer _) maxItems) =
  withPrompt promptRenderer
  where
    withPrompt (PromptRenderer acquire release _) =
      bracket acquire release (const run)
    run =
      menuResult =<< quitReason <$> withMergedSources consumer 64 (menuSources promptConfig items)
    consumer =
      evalStateC initial (awaitForever (updateMenu handle)) .| iterM render .| menuTerminator .| Conduit.last
    initial =
      set Menu.maxItems maxItems def
    quitReason =
      fromMaybe QuitReason.NoOutput

nvimMenu ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  ConduitT () [MenuItem i] m () ->
  (MenuUpdate m a i -> m (MenuAction m a, Menu i)) ->
  PromptConfig m ->
  Maybe Int ->
  m (MenuResult a)
nvimMenu options items handle promptConfig maxItems =
  run =<< showInScratch [] options
  where
    run scratch = do
      windowSetOption (scratchWindow scratch) "cursorline" (toMsgpack True)
      runMenu $ MenuConfig items (MenuConsumer handle) (render scratch) promptConfig maxItems
    render =
      renderNvimMenu (ensureSize options)
    ensureSize =
      over ScratchOptions.size (<|> Just 1)

strictNvimMenu ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  [MenuItem i] ->
  (MenuUpdate m a i -> m (MenuAction m a, Menu i)) ->
  PromptConfig m ->
  Maybe Int ->
  m (MenuResult a)
strictNvimMenu options items =
  nvimMenu (ensureSize options) (yield items)
  where
    ensureSize =
      over ScratchOptions.size (<|> Just (length items))
