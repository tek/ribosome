module Ribosome.Menu.Nvim where

import Control.Lens ((?~), (^.))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Lens ((<|>~))
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (MenuStack)
import Ribosome.Menu.Data.NvimMenuConfig (NvimMenuConfig (NvimMenuConfig))
import Ribosome.Menu.Data.PromptQuit (PromptQuit)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Filters (fuzzyMonotonic)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Main (menu)
import Ribosome.Menu.Prompt (defaultPromptConfig)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptListening)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

type NvimMenuStack i a res =
  MenuStack i ++ [
    MenuConsumer a,
    Settings !! SettingError,
    Scratch,
    Rpc,
    Rpc !! RpcError,
    Sync PromptQuit,
    Sync PromptListening,
    Log,
    Mask res,
    Race,
    Embed IO,
    Final IO
  ]

nvimMenuWith ::
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  PromptConfig ->
  NvimMenuConfig i
nvimMenuWith options items prompt =
  NvimMenuConfig (MenuConfig items Nothing prompt) (ensureSize options)
  where
    ensureSize =
      #size <|>~ Just 1

nvimMenu ::
  Members [Sync PromptQuit, Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  SerialT IO (MenuItem i) ->
  Sem r (NvimMenuConfig i)
nvimMenu items = do
  prompt <- defaultPromptConfig []
  pure (nvimMenuWith (scratch "ribosome-menu") items prompt)

staticNvimMenuWith ::
  ScratchOptions ->
  [MenuItem i] ->
  PromptConfig ->
  NvimMenuConfig i
staticNvimMenuWith options items prompt =
  setFilter (nvimMenuWith (ensureSize options) (Stream.fromList items) prompt)
  where
    setFilter =
      #menu . #itemFilter ?~ fuzzyMonotonic
    ensureSize =
      #size <|>~ Just (length items)

staticNvimMenu ::
  Members [Sync PromptQuit, Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  [MenuItem i] ->
  Sem r (NvimMenuConfig i)
staticNvimMenu items = do
  prompt <- defaultPromptConfig []
  pure (staticNvimMenuWith (scratch "ribosome-menu") items prompt)

staticNvimMenu_ ::
  [MenuItem i] ->
  NvimMenuConfig i
staticNvimMenu_ items =
  staticNvimMenuWith (scratch "ribosome-menu") items (PromptConfig Nothing [])

interpretNvimMenu ::
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch, Log, Resource, Embed IO, Final IO] r =>
  NvimMenuConfig i ->
  InterpretersFor [PromptEvents, Scoped ScratchId (MenuRenderer i), Scoped NvimPromptResources PromptRenderer] r
interpretNvimMenu (NvimMenuConfig config scratchOptions) =
  interpretPromptRendererNvim .
  interpretMenuRendererNvim scratchOptions .
  interpretPromptEventsDefault (config ^. #prompt . #flags)

runNvimMenu ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  NvimMenuConfig i ->
  Sem r (MenuResult a)
runNvimMenu conf =
  interpretNvimMenu conf do
    menu (conf ^. #menu)
