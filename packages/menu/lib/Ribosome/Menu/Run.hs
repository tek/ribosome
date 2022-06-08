module Ribosome.Menu.Run where

import Control.Lens ((%~), (<>~), (^.))
import qualified Data.Text as Text
import Polysemy.Conc (interpretAtomic, interpretSync)
import qualified Polysemy.Log as Log
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (nvimWinGetConfig, vimGetWindows, windowSetOption)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (MenuStateStack)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.PromptRenderer (NvimPrompt, PromptRenderer)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Main (runMenu)
import Ribosome.Menu.Nvim (menuSyntax, nvimMenuRenderer)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptListening, hoistPromptConfig)
import qualified Ribosome.Settings as Settings

isFloat ::
  Member (Rpc !! RpcError) r =>
  Window ->
  Sem r Bool
isFloat win =
  False <! (check <$> nvimWinGetConfig win)
  where
    check (WindowConfig relative _ _) =
      not (Text.null relative)

closeFloats ::
  Members [Rpc, Rpc !! RpcError] r =>
  Sem r ()
closeFloats = do
  traverse_ closeWindow =<< filterM isFloat =<< vimGetWindows

nvimMenuWith ::
  ∀ i a pres mres r .
  Show a =>
  Members [Scoped pres PromptRenderer, Scratch, Rpc, Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Sync PromptListening, Log, Mask mres, Resource, Race, Embed IO, Final IO] r =>
  MenuItemFilter i ->
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ PromptRenderer : AtomicState NvimMenuState : r) ->
  PromptConfig r ->
  Sem r (MenuResult a)
nvimMenuWith itemFilter options items consumer promptConfig = do
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  run =<< Scratch.open (withSyntax (ensureSize options))
  where
    run scratch = do
      windowSetOption (scratch ^. #window) "cursorline" True !>> Log.debug "Failed to set cursorline"
      interpretAtomic (def :: NvimMenuState) do
        runMenu consumer (MenuConfig items itemFilter (nvimMenuRenderer scratch) (hoistPromptConfig raise promptConfig))
    ensureSize =
      #size %~ (<|> Just 1)
    withSyntax =
      #syntax <>~ [menuSyntax]

nvimMenu ::
  ∀ i a res r .
  Show a =>
  Members [Scratch, Rpc, Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ PromptRenderer : AtomicState NvimMenuState : NvimPrompt : Sync PromptListening : r) ->
  PromptConfig (NvimPrompt : Sync PromptListening : r) ->
  Sem r (MenuResult a)
nvimMenu scro items consumer conf =
  interpretSync $ interpretPromptRendererNvim do
    nvimMenuWith (fuzzyItemFilter True) scro items consumer conf

staticNvimMenuWith ::
  Show a =>
  Members [Scoped pres PromptRenderer, Scratch, Rpc !! RpcError, Rpc, Settings !! SettingError] r =>
  Members [Sync PromptListening, Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  MenuItemFilter i ->
  ScratchOptions ->
  [MenuItem i] ->
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ PromptRenderer : AtomicState NvimMenuState : r) ->
  PromptConfig r ->
  Sem r (MenuResult a)
staticNvimMenuWith itemFilter options items =
  nvimMenuWith itemFilter (ensureSize options) (Stream.fromList items)
  where
    ensureSize =
      #size %~ (<|> Just (length items))

staticNvimMenu ::
  Show a =>
  Members [Scratch, Rpc !! RpcError, Rpc, Settings !! SettingError] r =>
  Members [Log, Mask res, Resource, Race, Embed IO, Final IO] r =>
  ScratchOptions ->
  [MenuItem i] ->
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ PromptRenderer : AtomicState NvimMenuState : NvimPrompt : Sync PromptListening : r) ->
  PromptConfig (NvimPrompt : Sync PromptListening : r) ->
  Sem r (MenuResult a)
staticNvimMenu scro items consumer conf =
  interpretSync $ interpretPromptRendererNvim do
    staticNvimMenuWith (fuzzyItemFilter True) scro items consumer conf
