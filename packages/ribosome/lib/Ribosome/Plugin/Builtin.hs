module Ribosome.Plugin.Builtin where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Data.Locks (WatcherLock)
import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
import Ribosome.Mapping (mappingHandler)
import Ribosome.Scratch (killScratchByName)
import Ribosome.Text (capitalize)
import Ribosome.VariableWatcher (variableWatcherHandler)

watcherEvents :: [Text]
watcherEvents =
  [
    "CmdlineLeave",
    "BufWinEnter",
    "VimEnter"
  ]

watcherRpc ::
  ∀ r .
  Members [AtomicState (Map WatchedVariable Object), Sync WatcherLock, Rpc !! RpcError, Resource] r =>
  Map WatchedVariable (Object -> Sem (Error HandlerError : r) ()) ->
  Text ->
  RpcHandler r
watcherRpc vars event =
  rpcAutocmd event Async def (variableWatcherHandler @(Error HandlerError : r) vars)

builtinHandlers ::
  ∀ r .
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  Members [AtomicState (Map WatchedVariable Object), Sync WatcherLock, Resource] r =>
  PluginName ->
  Map MappingIdent (Sem (Error HandlerError : r) ()) ->
  Map WatchedVariable (Object -> Sem (Error HandlerError : r) ()) ->
  [RpcHandler r]
builtinHandlers (PluginName name) maps vars =
  [
    rpcFunction [exon|#{capitalize name}DeleteScratch|] Async (killScratchByName @(Error HandlerError : r)),
    rpcFunction [exon|#{capitalize name}Mapping|] Async (mappingHandler maps)
  ] <> (watcherRpc vars <$> watcherEvents)
