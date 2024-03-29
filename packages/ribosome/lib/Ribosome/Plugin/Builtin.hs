-- |Interceptor that adds internal RPC handlers to the host.
module Ribosome.Plugin.Builtin where

import Exon (exon)
import Prelude hiding (group)

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
import Ribosome.Host.Data.RpcType (AutocmdGroup (AutocmdGroup), AutocmdOptions (target), AutocmdPatterns, group)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcAutocmd, rpcFunction)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Text (capitalize)

-- |The set of autocmds that should trigger an update in 'VariableWatcher'.
watcherEvents :: [(Text, AutocmdPatterns)]
watcherEvents =
  [
    ("CmdlineLeave", def),
    ("BufWinEnter", def),
    ("VimEnter", def),
    ("User", "RibosomeUpdateVariables")
  ]

-- |Run 'VariableWatcher.update' and restop errors.
updateVar ::
  Member (VariableWatcher !! Report) r =>
  Handler r ()
updateVar =
  restop VariableWatcher.update

-- |Declare an autocmd that triggers the variable watcher.
watcherRpc ::
  ∀ r .
  Member (VariableWatcher !! Report) r =>
  PluginName ->
  Text ->
  AutocmdPatterns ->
  RpcHandler r
watcherRpc (PluginName name) event pat =
  rpcAutocmd (RpcName method) Async (fromText event) def { target = Right pat, group = Just (AutocmdGroup name) } do
    updateVar
  where
    method =
      [exon|#{capitalize name}VariableChanged#{event}|]

-- |Delete a scratch buffer.
deleteScratch ::
  Member (Scratch !! RpcError) r =>
  ScratchId ->
  Handler r ()
deleteScratch =
  resumeReport . Scratch.delete

-- |The name for the handler that is triggered by a scratch buffer being deleted.
deleteName :: PluginName -> RpcName
deleteName (PluginName name) =
  RpcName [exon|#{capitalize name}DeleteScratch|]

-- |A set of 'RpcHandler's for internal tasks.
builtinHandlers ::
  ∀ r .
  Members [Scratch !! RpcError, VariableWatcher !! Report] r =>
  PluginName ->
  [RpcHandler r]
builtinHandlers name =
  rpcFunction (deleteName name) Async deleteScratch : (uncurry (watcherRpc name) <$> watcherEvents)

-- |The dependencies of the builtin handlers.
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

-- |Add builtin handlers to 'Handlers' without removing the effect from the stack.
interceptHandlersBuiltin ::
  Members BuiltinHandlersDeps r =>
  Sem r a ->
  Sem r a
interceptHandlersBuiltin sem = do
  name <- ask
  withHandlers (builtinHandlers name) sem
