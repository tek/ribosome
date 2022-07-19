module Ribosome.Plugin.Builtin where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.ScratchId (ScratchId)
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import qualified Ribosome.Effect.VariableWatcher as VariableWatcher
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Data.RpcType as AutocmdOptions
import Ribosome.Host.Data.RpcType (AutocmdEvent (unAutocmdEvent))
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
import Ribosome.Host.Interpreter.Handlers (interceptHandlers)
import Ribosome.Text (capitalize)

watcherEvents :: [AutocmdEvent]
watcherEvents =
  [
    "CmdlineLeave",
    "BufWinEnter",
    "VimEnter"
  ]

updateVar ::
  Member (VariableWatcher !! HandlerError) r =>
  Handler r ()
updateVar =
  restop VariableWatcher.update

watcherRpc ::
  ∀ r .
  Member (VariableWatcher !! HandlerError) r =>
  PluginName ->
  AutocmdEvent ->
  RpcHandler r
watcherRpc (PluginName name) event =
  rpcAutocmd (RpcName method) Async event def { AutocmdOptions.group = Just name } do
    updateVar
  where
    method =
      [exon|#{capitalize name}VariableChanged#{unAutocmdEvent event}|]

deleteScratch ::
  Member (Scratch !! RpcError) r =>
  ScratchId ->
  Handler r ()
deleteScratch =
  resumeHandlerError . Scratch.kill

deleteName :: PluginName -> RpcName
deleteName (PluginName name) =
  RpcName [exon|#{capitalize name}DeleteScratch|]

builtinHandlers ::
  ∀ r .
  Members [Scratch !! RpcError, VariableWatcher !! HandlerError] r =>
  PluginName ->
  [RpcHandler r]
builtinHandlers name =
    rpcFunction (deleteName name) Async deleteScratch : (watcherRpc name <$> watcherEvents)

interceptHandlersBuiltin ::
  Members [VariableWatcher !! HandlerError, Handlers !! HandlerError] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Reader PluginName, Error BootError, Log] r =>
  Sem r a ->
  Sem r a
interceptHandlersBuiltin sem = do
  name <- ask
  interceptHandlers (builtinHandlers name) sem
