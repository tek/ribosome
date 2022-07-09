module Ribosome.Menu.Nvim where

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
import Ribosome.Menu.Data.NvimMenuConfig (NvimMenuConfig (NvimMenuConfig))
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Effect.PromptStream (PromptStream)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.MenuState (MenuStack)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Main (menu)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

type NvimMenuStack i a res =
  MenuStack i ++ [
    MenuStream i,
    PromptStream,
    MenuConsumer a,
    PromptInput,
    Settings !! SettingError,
    Scratch,
    Rpc,
    Rpc !! RpcError,
    Log,
    ChronosTime,
    Mask res,
    Race,
    Resource,
    Embed IO,
    Final IO
  ]

menuScratch :: ScratchOptions
menuScratch =
  scratch "ribosome-menu"

menuScratchSized :: Int -> ScratchOptions
menuScratchSized n =
  menuScratch & #size ?~ n

streamMenuScratch :: ScratchOptions
streamMenuScratch =
  menuScratchSized 1

ensureSize :: Int -> ScratchOptions -> ScratchOptions
ensureSize n =
  #size <|>~ Just n

nvimMenuWith ::
  ScratchOptions ->
  SerialT IO (MenuItem i) ->
  NvimMenuConfig i
nvimMenuWith options items =
  NvimMenuConfig (MenuConfig items) (ensureSize 1 options)

nvimMenu ::
  SerialT IO (MenuItem i) ->
  NvimMenuConfig i
nvimMenu items = do
  nvimMenuWith menuScratch items

staticNvimMenuWith ::
  ScratchOptions ->
  [MenuItem i] ->
  NvimMenuConfig i
staticNvimMenuWith options items =
  nvimMenuWith (ensureSize (length items) options) (Stream.fromList items)

staticNvimMenu ::
  [MenuItem i] ->
  NvimMenuConfig i
staticNvimMenu items =
  staticNvimMenuWith menuScratch items

interpretNvimMenu ::
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch, Log, Resource, Embed IO, Final IO] r =>
  ScratchOptions ->
  InterpretersFor [Scoped ScratchId (MenuRenderer i), Scoped NvimPromptResources PromptRenderer] r
interpretNvimMenu scratchOptions =
  interpretPromptRendererNvim .
  interpretMenuRendererNvim scratchOptions

runNvimMenu ::
  ∀ a i res r .
  Show a =>
  Members (NvimMenuStack i a res) r =>
  NvimMenuConfig i ->
  Sem r (MenuResult a)
runNvimMenu conf =
  interpretNvimMenu (conf ^. #scratch) do
    menu
