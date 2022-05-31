module Ribosome.Menu.Run where

import Control.Lens ((%~), (<>~), (^.))
import qualified Data.Text as Text
import Polysemy.Conc (interpretAtomic, interpretSync)
import qualified Polysemy.Log as Log
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Api.Window (closeWindow)
import qualified Ribosome.Config.Settings as Settings
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
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
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.Main (menuMain)
import Ribosome.Menu.Nvim (menuSyntax, nvimMenuRenderer)
import qualified Ribosome.Menu.Prompt.Data.PromptConfig as PromptConfig
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptListening, hoistPromptConfig)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))

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
  Members [Sync PromptListening, Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  MenuConfig r i a ->
  Sem r (MenuResult a)
runMenu config =
  bracketPrompt (config ^. MenuConfig.prompt . PromptConfig.render)
  where
    bracketPrompt (PromptRenderer acquire release _) =
      bracket acquire release \ _ -> menuMain config

nvimMenuWith ::
  ∀ i a res r .
  Members [Scratch, Rpc, Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Sync PromptListening, Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  MenuItemFilter i ->
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  MenuConsumer i r a ->
  PromptConfig r ->
  Sem r (MenuResult a)
nvimMenuWith itemFilter options items consumer promptConfig = do
  _ :: Int <- vimCallFunction "inputsave" []
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  run =<< Scratch.open (withSyntax (ensureSize options))
  where
    run scratch = do
      windowSetOption (scratch ^. #window) "cursorline" (toMsgpack True) !>> Log.debug "Failed to set cursorline"
      interpretAtomic (def :: NvimMenuState) do
        runMenu (MenuConfig items itemFilter (hoistMenuConsumer raise (insertAt @5) consumer) (nvimMenuRenderer scratch) (hoistPromptConfig raise promptConfig))
    ensureSize =
      #size %~ (<|> Just 1)
    withSyntax =
      #syntax <>~ [menuSyntax]

nvimMenu ::
  ∀ i a res r .
  Members [Scratch, Rpc, Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  MenuConsumer i (Sync PromptListening : r) a ->
  PromptConfig (Sync PromptListening : r) ->
  Sem r (MenuResult a)
nvimMenu scro items consumer conf =
  interpretSync do
    nvimMenuWith (fuzzyItemFilter True) scro items consumer conf

staticNvimMenuWith ::
  Members [Scratch, Rpc !! RpcError, Rpc, Settings !! SettingError] r =>
  Members [Sync PromptListening, Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  MenuItemFilter i ->
  ScratchOptions ->
  [MenuItem i] ->
  MenuConsumer i r a ->
  PromptConfig r ->
  Sem r (MenuResult a)
staticNvimMenuWith itemFilter options items =
  nvimMenuWith itemFilter (ensureSize options) (Stream.fromList items)
  where
    ensureSize =
      #size %~ (<|> Just (length items))

staticNvimMenu ::
  Members [Scratch, Rpc !! RpcError, Rpc, Settings !! SettingError] r =>
  Members [Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  ScratchOptions ->
  [MenuItem i] ->
  MenuConsumer i (Sync PromptListening : r) a ->
  PromptConfig (Sync PromptListening : r) ->
  Sem r (MenuResult a)
staticNvimMenu scro items consumer conf =
  interpretSync do
    staticNvimMenuWith (fuzzyItemFilter True) scro items consumer conf
