module Ribosome.Menu.Run where

import Control.Concurrent.STM.TMChan (TMChan, writeTMChan)
import Control.Exception.Lifted (bracket)
import Control.Lens (over)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as Text
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (SerialT)

import Ribosome.Api.Window (closeWindow)
import Ribosome.Config.Setting (settingOr)
import qualified Ribosome.Config.Settings as Settings
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Scratch (scratchWindow)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (size, syntax)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Log (showDebug)
import Ribosome.Menu.Data.Menu (Menu)
import qualified Ribosome.Menu.Data.Menu as Menu (maxItems)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction (..))
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import qualified Ribosome.Menu.Data.MenuConfig as MenuConfig (prompt)
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer (MenuConsumer))
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QuitReason)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent (..))
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason (..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem (_text))
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent (..))
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult (..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate (MenuUpdate))
import Ribosome.Menu.Nvim (menuSyntax, renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import qualified Ribosome.Menu.Prompt.Data.PromptConfig as PromptConfig (render)
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed)
import qualified Ribosome.Menu.Prompt.Data.PromptConsumed as PromptConsumed (PromptConsumed (..))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate (PromptConsumerUpdate))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent (..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import Ribosome.Menu.Prompt.Run (promptStream)
import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinGetConfig, vimCallFunction, vimGetWindows, windowSetOption)
import Ribosome.Scratch (showInScratch)

promptEvent ::
  PromptEvent ->
  Prompt ->
  PromptConsumed ->
  MenuEvent m a i
promptEvent _ (Prompt _ PromptState.Quit _ _) _ =
  MenuEvent.Quit QuitReason.Aborted
promptEvent (PromptEvent.Character a) prompt PromptConsumed.No =
  MenuEvent.Mapping a prompt
promptEvent (PromptEvent.Character _) prompt@(Prompt _ PromptState.Insert _ _) _ =
  MenuEvent.PromptChange prompt
promptEvent (PromptEvent.Character _) prompt PromptConsumed.Yes =
  MenuEvent.PromptChange prompt
promptEvent (PromptEvent.Set _) prompt _ =
  MenuEvent.PromptChange prompt
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
  TMChan PromptEvent ->
  MenuConsumer m a i ->
  Either PromptConsumerUpdate [MenuItem i] ->
  SerialT (StateT (Menu i) m) (MenuRenderEvent m a i)
updateMenu backchannel (MenuConsumer consumer) input = do
  showDebug "menu update:" (fmap MenuItem._text <$> input)
  action <- lift . stateM $ lift . consumer . MenuUpdate (menuEvent input)
  showDebug "menu action:" action
  case action of
    MenuAction.Continue ->
      Streamly.nil
    MenuAction.Execute thunk ->
      Streamly.nilM (lift thunk)
    MenuAction.Render changed ->
      Streamly.fromEffect (gets (MenuRenderEvent.Render changed))
    MenuAction.UpdatePrompt prompt ->
      Streamly.nilM (atomically (writeTMChan backchannel (PromptEvent.Set prompt)))
    MenuAction.Quit reason ->
      Streamly.fromPure (MenuRenderEvent.Quit reason)

menuTerminator ::
  MenuRenderEvent m a i ->
  Maybe (QuitReason m a)
menuTerminator = \case
  MenuRenderEvent.Quit reason ->
    Just reason
  _ ->
    Nothing

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

menuMain ::
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MenuConfig m a i ->
  m (QuitReason m a)
menuMain (MenuConfig items handle render promptConfig maxItems) = do
  (backchannel, promptEvents) <- promptStream promptConfig
  let
    source =
      Streamly.parallel (Left <$> promptEvents) (Right <$> items)
  Streamly.headElse QuitReason.NoOutput (Streamly.mapMaybe menuTerminator (consumer backchannel source))
  where
    consumer backchannel source =
      Streamly.trace render $
      Streamly.evalStateT (pure initial) (menuHandler backchannel =<< Streamly.liftInner source)
    initial =
      def & Menu.maxItems .~ maxItems
    menuHandler backchannel =
      updateMenu backchannel handle

isFloat ::
  NvimE e m =>
  Window ->
  m Bool
isFloat =
  fmap (check . fromMsgpack . toMsgpack) . nvimWinGetConfig
  where
    check (Right (WindowConfig relative _ _)) =
      not (Text.null relative)
    check _ =
      False

closeFloats ::
  NvimE e m =>
  m ()
closeFloats = do
  traverse_ closeWindow =<< filterM isFloat =<< vimGetWindows

runMenu ::
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MenuConfig m a i ->
  m (MenuResult a)
runMenu config =
  bracketPrompt (config ^. MenuConfig.prompt . PromptConfig.render)
  where
    bracketPrompt (PromptRenderer acquire release _) =
      bracket acquire release (const run)
    run =
      menuResult =<< menuMain config

nvimMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  SerialT m [MenuItem i] ->
  (MenuUpdate m a i -> m (MenuAction m a, Menu i)) ->
  PromptConfig m ->
  Maybe Int ->
  m (MenuResult a)
nvimMenu options items handle promptConfig maxItems = do
  _ :: Int <- vimCallFunction "inputsave" []
  whenM (settingOr True Settings.menuCloseFloats) closeFloats
  run =<< showInScratch @[] [] (withSyntax (ensureSize options))
  where
    run scratch = do
      windowSetOption (scratchWindow scratch) "cursorline" (toMsgpack True)
      runMenu $ MenuConfig items (MenuConsumer handle) (render scratch) promptConfig maxItems
    render =
      renderNvimMenu options
    ensureSize =
      over ScratchOptions.size (<|> Just 1)
    withSyntax =
      over ScratchOptions.syntax (++ [menuSyntax])

strictNvimMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  [MenuItem i] ->
  (MenuUpdate m a i -> m (MenuAction m a, Menu i)) ->
  PromptConfig m ->
  Maybe Int ->
  m (MenuResult a)
strictNvimMenu options items =
  nvimMenu (ensureSize options) (Streamly.fromPure items)
  where
    ensureSize =
      over ScratchOptions.size (<|> Just (length items))
