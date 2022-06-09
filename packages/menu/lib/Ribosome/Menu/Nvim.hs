module Ribosome.Menu.Nvim where

import Control.Lens ((%~))
import Polysemy.Conc (interpretSync)
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
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (MenuStack)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.MenuRenderer (NvimRenderer)
import Ribosome.Menu.Effect.PromptRenderer (NvimPrompt)
import Ribosome.Menu.Filters (fuzzy)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Main (menu)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptListening)

nvimMenuDef ::
  ∀ a i res r .
  Show a =>
  Members (MenuStack i) r =>
  Members [MenuConsumer a, Settings !! SettingError, Scratch, Rpc, Rpc !! RpcError] r =>
  Members [Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  PromptConfig (NvimRenderer i : NvimPrompt : r) ->
  Sem r (MenuResult a)
nvimMenuDef options items promptConfig =
  interpretPromptRendererNvim $ interpretMenuRendererNvim (ensureSize options) do
    menu (MenuConfig items fuzzy promptConfig)
  where
    ensureSize =
      #size %~ (<|> Just 1)

staticNvimMenuDef ::
  ∀ a i res r .
  Show a =>
  Members (MenuStack i) r =>
  Members [MenuConsumer a, Settings !! SettingError, Scratch, Rpc, Rpc !! RpcError] r =>
  Members [Sync PromptListening, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  ScratchOptions ->
  [MenuItem i] ->
  PromptConfig (NvimRenderer i : NvimPrompt : Sync PromptListening : r) ->
  Sem r (MenuResult a)
staticNvimMenuDef options items promptConfig =
  interpretSync $ interpretPromptRendererNvim $ interpretMenuRendererNvim (ensureSize options) do
    menu (MenuConfig (Stream.fromList items) fuzzy promptConfig)
  where
    ensureSize =
      #size %~ (<|> Just (length items))
