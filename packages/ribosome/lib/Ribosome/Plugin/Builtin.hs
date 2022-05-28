module Ribosome.Plugin.Builtin where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.ScratchState (ScratchId)
import qualified Ribosome.Effect.BuiltinHandlers as BuiltinHandlers
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Data.RpcType (AutocmdEvent (unAutocmdEvent))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
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

killScratchHandler ::
  Member (Scratch !! RpcError) r =>
  ScratchId ->
  Handler r ()
killScratchHandler =
  resumeHandlerError . Scratch.kill

builtinHandlers ::
  ∀ r .
  Members [Scratch !! RpcError, BuiltinHandlers !! HandlerError, Rpc !! RpcError] r =>
  Members [Log, Resource, Race] r =>
  PluginName ->
  [RpcHandler r]
builtinHandlers pn@(PluginName name) =
  [
    rpcFunction [exon|#{capitalize name}DeleteScratch|] Async killScratchHandler,
    rpcFunction [exon|#{capitalize name}Mapping|] Async (restop @_ @_ @(Stop _ : r) . BuiltinHandlers.mapping)
  ] <> (watcherRpc pn <$> watcherEvents)
