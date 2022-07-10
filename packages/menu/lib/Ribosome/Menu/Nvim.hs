module Ribosome.Menu.Nvim where

import Conc (PScoped)
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
import Ribosome.Menu.Data.NvimMenuConfig (NvimMenuConfig (NvimMenuConfig))
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources)

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
nvimMenu items =
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
  Members [Rpc, Rpc !! RpcError, Settings !! SettingError, Scratch, Log, Resource, Embed IO] r =>
  InterpretersFor [PScoped ScratchOptions ScratchId (MenuRenderer i), Scoped NvimPromptResources PromptRenderer] r
interpretNvimMenu =
  interpretPromptRendererNvim .
  interpretMenuRendererNvim
