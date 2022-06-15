module Ribosome.Remote where

import Ribosome.Data.PluginConfig (PluginConfig)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack, runCli)
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

runNvimPlugin ::
  ∀ r .
  HigherOrder (NvimPlugin : r) RemoteStack =>
  InterpretersFor (NvimPlugin : r) RemoteStack ->
  Sem BasicPluginStack ()
runNvimPlugin handlers =
  interpretPluginRemote (handlers runPluginHostRemote)

runNvimPlugin_ ::
  InterpreterFor NvimPlugin RemoteStack ->
  Sem BasicPluginStack ()
runNvimPlugin_ =
  runNvimPlugin @'[]

runNvimHandlers ::
  ∀ r .
  HigherOrder r RemoteStack =>
  InterpretersFor r RemoteStack ->
  [RpcHandler (r ++ RemoteStack)] ->
  Sem BasicPluginStack ()
runNvimHandlers effs handlers =
  runNvimPlugin @r (effs . rpcHandlers handlers)

runNvimHandlers_ ::
  [RpcHandler RemoteStack] ->
  Sem BasicPluginStack ()
runNvimHandlers_ =
  runNvimHandlers @'[] id

runNvimPluginIO ::
  ∀ r .
  HigherOrder (NvimPlugin : r) RemoteStack =>
  PluginConfig ->
  InterpretersFor (NvimPlugin : r) RemoteStack ->
  IO ()
runNvimPluginIO conf handlers =
  runCli conf (runNvimPlugin @r handlers)

runNvimPluginIO_ ::
  PluginConfig ->
  InterpreterFor NvimPlugin RemoteStack ->
  IO ()
runNvimPluginIO_ =
  runNvimPluginIO @'[]

runNvimHandlersIO ::
  ∀ r .
  HigherOrder r RemoteStack =>
  PluginConfig ->
  InterpretersFor r RemoteStack ->
  [RpcHandler (r ++ RemoteStack)] ->
  IO ()
runNvimHandlersIO conf effs handlers =
  runCli conf (runNvimHandlers @r effs handlers)

runNvimHandlersIO_ ::
  PluginConfig ->
  [RpcHandler RemoteStack] ->
  IO ()
runNvimHandlersIO_ conf =
  runNvimHandlersIO @'[] conf id
