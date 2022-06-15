module Ribosome.Menu.Nvim where

import Control.Lens ((%~), (^.))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (MenuStack)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Filters (fuzzy, fuzzyMonotonic)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Main (menu)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptListening)

type NvimMenuStack i a res =
  MenuStack i ++ [
    MenuConsumer a,
    Settings !! SettingError,
    Scratch,
    Rpc,
    Rpc !! RpcError,
    Sync PromptListening,
    Log,
    Mask res,
    Race,
    Resource,
    Embed IO,
    Final IO
  ]

nvimMenu ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  ScratchOptions ->
  MenuConfig i ->
  Sem r (MenuResult a)
nvimMenu options config =
  interpretPromptRendererNvim $ interpretMenuRendererNvim (ensureSize options) $ interpretPromptEventsDefault flags do
    menu config
  where
    flags =
      config ^. #prompt . #flags
    ensureSize =
      #size %~ (<|> Just 1)

nvimMenuDef ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  PromptConfig ->
  Sem r (MenuResult a)
nvimMenuDef options items promptConfig =
  nvimMenu options (MenuConfig items fuzzy promptConfig)

staticNvimMenu ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  MenuItemFilter i ->
  ScratchOptions ->
  [MenuItem i] ->
  PromptConfig ->
  Sem r (MenuResult a)
staticNvimMenu itemFilter options items promptConfig =
  nvimMenu (ensureSize options) (MenuConfig (Stream.fromList items) itemFilter promptConfig)
  where
    ensureSize =
      #size %~ (<|> Just (length items))

staticNvimMenuDef ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  ScratchOptions ->
  [MenuItem i] ->
  PromptConfig ->
  Sem r (MenuResult a)
staticNvimMenuDef =
  staticNvimMenu fuzzyMonotonic
