module Ribosome.Plugin.Builtin where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Scratch (Scratch)
import qualified Ribosome.Effect.BuiltinHandlers as BuiltinHandlers
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Data.RpcType (AutocmdEvent (unAutocmdEvent))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
import Ribosome.Scratch (killScratchByName)
import Ribosome.Text (capitalize)

watcherEvents :: [AutocmdEvent]
watcherEvents =
  [
    "CmdlineLeave",
    "BufWinEnter",
    "VimEnter"
  ]

watcherRpc ::
  ∀ r .
  Members [BuiltinHandlers !! HandlerError, Rpc !! RpcError, Resource, Race] r =>
  PluginName ->
  AutocmdEvent ->
  RpcHandler r
watcherRpc (PluginName name) event =
  rpcAutocmd method event def (restop @_ @_ @(Stop _ : r) BuiltinHandlers.variable)
  where
    method =
      [exon|#{capitalize name}VariableChanged#{unAutocmdEvent event}|]

builtinHandlers ::
  ∀ r .
  Members [BuiltinHandlers !! HandlerError, Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  Members [Resource, Race] r =>
  PluginName ->
  [RpcHandler r]
builtinHandlers pn@(PluginName name) =
  [
    rpcFunction [exon|#{capitalize name}DeleteScratch|] Async (killScratchByName @(Stop HandlerError : r)),
    rpcFunction [exon|#{capitalize name}Mapping|] Async (restop @_ @_ @(Stop _ : r) . BuiltinHandlers.mapping)
  ] <> (watcherRpc pn <$> watcherEvents)
