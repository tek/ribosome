module Ribosome.Remote where

import Ribosome.Data.PluginConfig (PluginConfig)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack, runBasicPluginStack)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers, sendNvimPlugin)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type HandlerStack =
  PluginEffects ++ RpcStack ++ RpcDeps

type RemoteStack =
  HandlerStack ++ BasicPluginStack

interpretRpcDeps ::
  Members IOStack r =>
  Member (Reader PluginName) r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealStdio

interpretPluginRemote ::
  Members BasicPluginStack r =>
  InterpretersFor HandlerStack r
interpretPluginRemote =
  interpretRpcDeps .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

runPluginHostRemote ::
  ∀ r .
  Member NvimPlugin r =>
  Members HandlerStack r =>
  Members BasicPluginStack r =>
  Sem r ()
runPluginHostRemote =
  sendNvimPlugin $
  interpretBuiltinHandlers $
  interceptHandlersBuiltin runHost

runPluginRemote ::
  InterpreterFor NvimPlugin RemoteStack ->
  Sem BasicPluginStack ()
runPluginRemote handlers =
  interpretPluginRemote $
  handlers runPluginHostRemote

runNvimPlugin ::
  InterpreterFor NvimPlugin RemoteStack ->
  Sem BasicPluginStack ()
runNvimPlugin =
  runPluginRemote

runNvimHandlers ::
  [RpcHandler RemoteStack] ->
  Sem BasicPluginStack ()
runNvimHandlers handlers =
  runPluginRemote (rpcHandlers handlers)

runNvimPluginIO ::
  PluginConfig ->
  InterpreterFor NvimPlugin RemoteStack ->
  IO ()
runNvimPluginIO conf handlers =
  runBasicPluginStack conf (runNvimPlugin handlers)

runNvimHandlersIO ::
  PluginConfig ->
  [RpcHandler RemoteStack] ->
  IO ()
runNvimHandlersIO conf handlers =
  runBasicPluginStack conf (runNvimHandlers handlers)
