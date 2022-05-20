module Ribosome.Plugin.Builtin where

import qualified Data.Map.Strict as Map
import Exon (exon)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Scratch (killScratchByName)
import Ribosome.Text (capitalize)

mappingHandler ::
  Map MappingIdent (Sem (Error HandlerError : r) ()) ->
  MappingIdent ->
  Sem (Error HandlerError : r) ()
mappingHandler maps i =
  join (note "No handler for this mapping" (Map.lookup i maps))

builtinHandlers ::
  ∀ r .
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  PluginName ->
  Map MappingIdent (Sem (Error HandlerError : r) ()) ->
  [RpcHandler r]
builtinHandlers (PluginName name) maps =
  [
    rpcFunction [exon|#{capitalize name}DeleteScratch|] Sync (killScratchByName @(Error HandlerError : r)),
    rpcFunction [exon|#{capitalize name}Mapping|] Sync (mappingHandler maps)
  ]
