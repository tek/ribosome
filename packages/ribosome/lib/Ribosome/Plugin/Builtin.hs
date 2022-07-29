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
import Ribosome.Host.Data.Report (Report, resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Data.RpcType as AutocmdOptions
import Ribosome.Host.Data.RpcType (
  AutocmdEvent (unAutocmdEvent),
  AutocmdGroup (AutocmdGroup),
  AutocmdOptions (pat),
  AutocmdPattern,
  )
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Text (capitalize)

watcherEvents :: [(AutocmdEvent, AutocmdPattern)]
watcherEvents =
  [
    ("CmdlineLeave", def),
    ("BufWinEnter", def),
    ("VimEnter", def),
    ("User", "RibosomeUpdateVariables")
  ]

updateVar ::
  Member (VariableWatcher !! Report) r =>
  Handler r ()
updateVar =
  restop VariableWatcher.update

watcherRpc ::
  ∀ r .
  Member (VariableWatcher !! Report) r =>
  PluginName ->
  AutocmdEvent ->
  AutocmdPattern ->
  RpcHandler r
watcherRpc (PluginName name) event pat =
  rpcAutocmd (RpcName method) Async event def { pat = pat } { AutocmdOptions.group = Just (AutocmdGroup name) } do
    updateVar
  where
    method =
      [exon|#{capitalize name}VariableChanged#{unAutocmdEvent event}|]

deleteScratch ::
  Member (Scratch !! RpcError) r =>
  ScratchId ->
  Handler r ()
deleteScratch =
  resumeReport . Scratch.kill

deleteName :: PluginName -> RpcName
deleteName (PluginName name) =
  RpcName [exon|#{capitalize name}DeleteScratch|]

builtinHandlers ::
  ∀ r .
  Members [Scratch !! RpcError, VariableWatcher !! Report] r =>
  PluginName ->
  [RpcHandler r]
builtinHandlers name =
    rpcFunction (deleteName name) Async deleteScratch : (uncurry (watcherRpc name) <$> watcherEvents)

type BuiltinHandlersDeps =
  [
    VariableWatcher !! Report,
    Handlers !! Report,
    Scratch !! RpcError,
    Rpc !! RpcError,
    Reader PluginName,
    Error BootError,
    Log
  ]

interceptHandlersBuiltin ::
  Members BuiltinHandlersDeps r =>
  Sem r a ->
  Sem r a
interceptHandlersBuiltin sem = do
  name <- ask
  withHandlers (builtinHandlers name) sem
