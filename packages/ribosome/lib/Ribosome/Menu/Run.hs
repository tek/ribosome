module Ribosome.Menu.Run where

import Control.Exception.Lifted (bracket)
import Control.Lens ((<>~))
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as Text
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Api.Window (closeWindow)
import Ribosome.Config.Setting (settingOr)
import qualified Ribosome.Config.Settings as Settings
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Scratch (scratchWindow)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import qualified Ribosome.Menu.Data.MenuConfig as MenuConfig
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Nvim (menuSyntax, nvimMenuRenderer)
import qualified Ribosome.Menu.Prompt.Data.PromptConfig as PromptConfig
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinGetConfig, vimCallFunction, vimGetWindows, windowSetOption)
import Ribosome.Scratch (showInScratch)

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
  MenuConfig m i a ->
  m (MenuResult a)
runMenu config =
  bracketPrompt (config ^. MenuConfig.prompt . PromptConfig.render)
  where
    bracketPrompt (PromptRenderer acquire release _) =
      bracket acquire release \ _ -> menuMain config

nvimMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  SerialT m (MenuItem i) ->
  MenuConsumer m i a ->
  PromptConfig m ->
  m (MenuResult a)
nvimMenu options items consumer promptConfig = do
  _ :: Int <- vimCallFunction "inputsave" []
  whenM (settingOr True Settings.menuCloseFloats) closeFloats
  run =<< showInScratch @[] [] (withSyntax (ensureSize options))
  where
    run scratch = do
      renderer <- nvimMenuRenderer options scratch
      windowSetOption (scratchWindow scratch) "cursorline" (toMsgpack True)
      runMenu (MenuConfig items fuzzyItemFilter consumer renderer promptConfig)
    ensureSize =
      ScratchOptions.size %~ (<|> Just 1)
    withSyntax =
      ScratchOptions.syntax <>~ [menuSyntax]

staticNvimMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  [MenuItem i] ->
  MenuConsumer m i a ->
  PromptConfig m ->
  m (MenuResult a)
staticNvimMenu options items =
  nvimMenu (ensureSize options) (Stream.fromList items)
  where
    ensureSize =
      ScratchOptions.size %~ (<|> Just (length items))
