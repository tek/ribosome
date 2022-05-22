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
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Data.RpcType (AutocmdEvent (unAutocmdEvent))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
import Ribosome.Mapping (mappingHandler)
import Ribosome.Scratch (killScratchByName)
import Ribosome.Text (capitalize)
import Ribosome.VariableWatcher (variableWatcherHandler)

watcherEvents :: [AutocmdEvent]
watcherEvents =
  [
    "CmdlineLeave",
    "BufWinEnter",
    "VimEnter"
  ]

watcherRpc ::
  ∀ r .
  Members [AtomicState (Map WatchedVariable Object), Sync WatcherLock, Rpc !! RpcError, Resource, Race] r =>
  Map WatchedVariable (Object -> Handler r ()) ->
  PluginName ->
  AutocmdEvent ->
  RpcHandler r
watcherRpc vars (PluginName name) event =
  rpcAutocmd method event def (variableWatcherHandler @(Error HandlerError : r) vars)
  where
    method =
      [exon|#{capitalize name}VariableChanged#{unAutocmdEvent event}|]

builtinHandlers ::
  ∀ r .
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  Members [AtomicState (Map WatchedVariable Object), Sync WatcherLock, Resource, Race] r =>
  PluginName ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  [RpcHandler r]
builtinHandlers pn@(PluginName name) maps vars =
  [
    rpcFunction [exon|#{capitalize name}DeleteScratch|] Async (killScratchByName @(Error HandlerError : r)),
    rpcFunction [exon|#{capitalize name}Mapping|] Async (mappingHandler maps)
  ] <> (watcherRpc vars pn <$> watcherEvents)
