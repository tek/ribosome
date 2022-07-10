module Ribosome.Menu.Interpreter.PromptRenderer where

import Conc (interpretScopedAs, interpretScopedResumable)

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer (RenderPrompt))
import Ribosome.Menu.Prompt.Nvim (NvimPromptResources, nvimAcquire, nvimRelease, nvimRenderPrompt)

interpretPromptRendererNull :: InterpreterFor (Scoped () PromptRenderer) r
interpretPromptRendererNull =
  interpretScopedAs unit \ () -> \case
    RenderPrompt _ -> unit

withNvimResources ::
  Members [Rpc !! RpcError, Resource] r =>
  (NvimPromptResources ->  Sem (Stop RpcError : r) a) ->
  Sem (Stop RpcError : r) a
withNvimResources =
  bracket (restop @_ @Rpc nvimAcquire) (restop . nvimRelease)

interpretPromptRendererNvim ::
  Members [Rpc !! RpcError, Resource] r =>
  InterpreterFor (Scoped NvimPromptResources PromptRenderer !! RpcError) r
interpretPromptRendererNvim =
  interpretScopedResumable withNvimResources \ _ -> \case
    RenderPrompt prompt ->
      restop (nvimRenderPrompt prompt)
