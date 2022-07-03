module Ribosome.Menu.Nvim where

import Control.Lens ((?~), (^.))
import Polysemy.Chronos (ChronosTime)
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
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Filters (fuzzyMonotonic)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Main (menu)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Prompt.Data.PromptListening (PromptListening)
import Ribosome.Menu.Prompt.Data.PromptQuit (PromptQuit)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

type NvimMenuStack i a res =
  MenuStack i ++ [
    MenuConsumer a,
    PromptInput,
    Settings !! SettingError,
    Scratch,
    Rpc,
    Rpc !! RpcError,
    Sync PromptQuit,
    Sync PromptListening,
    Log,
    ChronosTime,
    Mask res,
    Race,
    Embed IO,
    Final IO
  ]

menuScratch :: ScratchOptions
menuScratch =
  scratch "ribosome-menu"

nvimMenuWith ::
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  [PromptFlag] ->
  NvimMenuConfig i
nvimMenuWith options items flags =
  NvimMenuConfig (MenuConfig items Nothing flags) (ensureSize options)
  where
    ensureSize =
      #size <|>~ Just 1

nvimMenu ::
  SerialT IO (MenuItem i) ->
  NvimMenuConfig i
nvimMenu items = do
  nvimMenuWith menuScratch items []

staticNvimMenuWith ::
  ScratchOptions ->
  [MenuItem i] ->
  [PromptFlag] ->
  NvimMenuConfig i
staticNvimMenuWith options items flags =
  setFilter (nvimMenuWith (ensureSize options) (Stream.fromList items) flags)
  where
    setFilter =
      #menu . #itemFilter ?~ fuzzyMonotonic
    ensureSize =
      #size <|>~ Just (length items)

staticNvimMenu ::
  [MenuItem i] ->
  NvimMenuConfig i
staticNvimMenu items =
  staticNvimMenuWith menuScratch items []

interpretNvimMenu ::
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch, Log, Resource, Embed IO, Final IO] r =>
  NvimMenuConfig i ->
  InterpretersFor [PromptEvents, Scoped ScratchId (MenuRenderer i), Scoped NvimPromptResources PromptRenderer] r
interpretNvimMenu (NvimMenuConfig config scratchOptions) =
  interpretPromptRendererNvim .
  interpretMenuRendererNvim scratchOptions .
  interpretPromptEventsDefault (config ^. #flags)

runNvimMenu ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  NvimMenuConfig i ->
  Sem r (MenuResult a)
runNvimMenu conf =
  interpretNvimMenu conf do
    menu (conf ^. #menu)
