module Ribosome.Plugin.Builtin where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Scratch (killScratchByName)
import Ribosome.Text (capitalize)

builtinHandlers ::
  ∀ r .
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  PluginName ->
  [RpcHandler r]
builtinHandlers (PluginName name) =
  [
    rpcFunction [exon|#{capitalize name}DeleteScratch|] Async (killScratchByName @(Error HandlerError : r))
  ]
