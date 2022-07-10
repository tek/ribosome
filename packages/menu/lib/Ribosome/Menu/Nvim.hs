module Ribosome.Menu.Nvim where

import Conc (PScoped)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Lens ((<|>~))
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

ensureSize :: Int -> ScratchOptions -> ScratchOptions
ensureSize n =
  #size <|>~ Just n

interpretNvimMenu ::
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Log, Resource, Embed IO] r =>
  InterpretersFor [
    PScoped ScratchOptions ScratchId (MenuRenderer i) !! RpcError,
    Scoped NvimPromptResources PromptRenderer !! RpcError
  ] r
interpretNvimMenu =
  interpretPromptRendererNvim .
  interpretMenuRendererNvim
