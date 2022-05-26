module Ribosome.Menu.Run where

import Control.Lens ((%~), (<>~), (^.))
import qualified Data.Text as Text
import Polysemy.Conc (interpretAtomic)
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Api.Window (closeWindow)
import qualified Ribosome.Config.Settings as Settings
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch, scratchWindow)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (nvimWinGetConfig, vimCallFunction, vimGetWindows, windowSetOption)
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import qualified Ribosome.Menu.Data.MenuConfig as MenuConfig
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer, hoistMenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Nvim (menuSyntax, nvimMenuRenderer)
import qualified Ribosome.Menu.Prompt.Data.PromptConfig as PromptConfig
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import Ribosome.Scratch (showInScratch)

isFloat ::
  Member Rpc r =>
  Window ->
  Sem r Bool
isFloat =
  fmap (check . fromMsgpack) . nvimWinGetConfig
  where
    check (Right (WindowConfig relative _ _)) =
      not (Text.null relative)
    check _ =
      False

closeFloats ::
  Member Rpc r =>
  Sem r ()
closeFloats = do
  traverse_ closeWindow =<< filterM isFloat =<< vimGetWindows

runMenu ::
  Members [Log, Resource, Race, Embed IO, Final IO] r =>
  MenuConfig r IO i a ->
  Sem r (MenuResult a)
runMenu config =
  bracketPrompt (config ^. MenuConfig.prompt . PromptConfig.render)
  where
    bracketPrompt (PromptRenderer acquire release _) =
      bracket (embed acquire) (embed . release) \ _ -> menuMain config

nvimMenu ::
  ∀ i a r .
  Members [Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource, Race, Embed IO, Final IO] r =>
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  MenuConsumer i r a ->
  PromptConfig IO ->
  Sem r (MenuResult a)
nvimMenu options items consumer promptConfig = do
  _ :: Int <- vimCallFunction "inputsave" []
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  run =<< showInScratch @[] [] (withSyntax (ensureSize options))
  where
    run scratch = do
      windowSetOption (scratchWindow scratch) "cursorline" (toMsgpack True)
      interpretAtomic (def :: NvimMenuState) do
        runMenu (MenuConfig items fuzzyItemFilter (hoistMenuConsumer raise (insertAt @5) consumer) (nvimMenuRenderer options scratch) promptConfig)
    ensureSize =
      ScratchOptions.size %~ (<|> Just 1)
    withSyntax =
      ScratchOptions.syntax <>~ [menuSyntax]

staticNvimMenu ::
  Members [Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource, Race, Embed IO, Final IO] r =>
  ScratchOptions ->
  [MenuItem i] ->
  MenuConsumer i r a ->
  PromptConfig IO ->
  Sem r (MenuResult a)
staticNvimMenu options items =
  nvimMenu (ensureSize options) (Stream.fromList items)
  where
    ensureSize =
      ScratchOptions.size %~ (<|> Just (length items))
