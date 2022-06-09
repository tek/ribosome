module Ribosome.Menu.Interpreter.PromptRenderer where

import Conc (interpretScoped, interpretScopedAs)

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer (RenderPrompt))
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources, nvimAcquire, nvimRelease, nvimRenderPrompt)

interpretPromptRendererNull :: InterpreterFor (Scoped () PromptRenderer) r
interpretPromptRendererNull =
  interpretScopedAs unit \ () -> \case
    RenderPrompt _ -> unit

withNvimResources ::
  Members [Rpc, Rpc !! RpcError, Resource] r =>
  (NvimPromptResources ->  Sem r a) ->
  Sem r a
withNvimResources =
  bracket nvimAcquire nvimRelease

interpretPromptRendererNvim ::
  Members [Rpc, Rpc !! RpcError, Resource] r =>
  InterpreterFor (Scoped NvimPromptResources PromptRenderer) r
interpretPromptRendererNvim =
  interpretScoped withNvimResources \ _ -> \case
    RenderPrompt prompt -> nvimRenderPrompt prompt
