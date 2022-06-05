module Ribosome.Remote where

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.MappingHandler (MappingHandler)
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.IOStack (BasicStack, IOStack, runBasicStack)
import Ribosome.Host.Interpreter.Handlers (interceptHandlers)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)
import Ribosome.Run (PluginEffects)

type BasicPluginRemoteStack =
  RpcStack ++ RpcDeps ++ '[Reader PluginName]

type HandlerDeps =
  PluginEffects ++ BasicPluginRemoteStack

type PluginRemoteStack =
  BuiltinHandlers !! HandlerError : HandlerDeps

type PluginRemoteIODeps =
  HandlerDeps ++ BasicStack

type PluginRemoteIOStack =
  VariableWatcher : MappingHandler : PluginRemoteIODeps

type PluginHandler r =
  Handler (PluginEffects ++ BasicPluginRemoteStack ++ r) ()

interpretRpcDeps ::
  Members IOStack r =>
  Member (Reader PluginName) r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealStdio

interpretPluginRemote ::
  Members BasicStack r =>
  PluginName ->
  InterpretersFor HandlerDeps r
interpretPluginRemote name =
  runReader name .
  interpretRpcDeps .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

runPluginHostRemote ::
  ∀ r .
  Members NvimPlugin' r =>
  Members HandlerDeps r =>
  Members BasicStack r =>
  PluginName ->
  Sem r ()
runPluginHostRemote name =
  interpretBuiltinHandlers $
  interceptHandlers (builtinHandlers name) runHost

runPluginRemote ::
  InterpretersFor NvimPlugin' PluginRemoteIODeps ->
  PluginName ->
  Sem BasicStack ()
runPluginRemote handlers name =
  interpretPluginRemote name $
  handlers $
  runPluginHostRemote name

runNvimPlugin ::
  InterpretersFor NvimPlugin' PluginRemoteIODeps ->
  PluginName ->
  Sem BasicStack ()
runNvimPlugin =
  runPluginRemote

runNvimHandlers ::
  [RpcHandler PluginRemoteIODeps] ->
  PluginName ->
  Sem BasicStack ()
runNvimHandlers handlers =
  runPluginRemote (rpcHandlers handlers)

runNvimPluginIO ::
  PluginConfig ->
  InterpretersFor NvimPlugin' PluginRemoteIODeps ->
  IO ()
runNvimPluginIO (PluginConfig name conf) handlers =
  runBasicStack conf (runNvimPlugin handlers name)

runNvimHandlersIO ::
  PluginConfig ->
  [RpcHandler PluginRemoteIODeps] ->
  IO ()
runNvimHandlersIO (PluginConfig name conf) handlers =
  runBasicStack conf (runNvimHandlers handlers name)
