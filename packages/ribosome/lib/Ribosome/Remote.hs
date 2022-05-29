module Ribosome.Remote where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (PluginEffects, interpretPluginEffects)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Embed (interpretCoreDeps)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Remote (IOStack, RemoteStack, interpretHostRemoteCore, runIOStack)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)

type PluginRemoteStack =
  PluginEffects ++ RemoteStack ++ '[Reader PluginName]

type PluginIOStack =
  PluginRemoteStack ++
  IOStack

type PluginHandler r =
  Handler (r ++ PluginIOStack) ()

interpretPluginRemote ::
  Members IOStack r =>
  PluginName ->
  HostConfig ->
  InterpretersFor PluginRemoteStack r
interpretPluginRemote name conf =
  runReader name .
  interpretCoreDeps conf .
  interpretUserErrorPrefixed .
  interpretHostRemoteCore .
  interpretPluginEffects

interpretNvimPlugin ::
  ∀ r .
  Members PluginIOStack (r ++ PluginIOStack) =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (r ++ PluginIOStack)] ->
  Sem (r ++ PluginIOStack) ()
interpretNvimPlugin name maps vars handlers =
  interpretBuiltinHandlers maps vars $
  interpretHandlers (builtinHandlers name <> hoistRpcHandlers raiseUnder handlers) runHost

runNvimPlugin ::
  ∀ r .
  Members PluginIOStack (r ++ PluginIOStack) =>
  PluginConfig ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (r ++ PluginIOStack)] ->
  InterpretersFor r PluginIOStack ->
  Sem IOStack ()
runNvimPlugin (PluginConfig name conf) maps vars handlers effs =
  interpretPluginRemote name conf $
  effs $
  interpretNvimPlugin @r name maps vars handlers

runNvimPlugin_ ::
  PluginConfig ->
  Map MappingIdent (PluginHandler '[]) ->
  Map WatchedVariable (Object -> PluginHandler '[]) ->
  [RpcHandler PluginIOStack] ->
  Sem IOStack ()
runNvimPlugin_ (PluginConfig name conf) maps vars handlers =
  interpretPluginRemote name conf $
  interpretNvimPlugin @'[] name maps vars handlers

runNvimPluginIO ::
  ∀ r .
  Members PluginIOStack (r ++ PluginIOStack) =>
  PluginConfig ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (r ++ PluginIOStack)] ->
  InterpretersFor r PluginIOStack ->
  IO ()
runNvimPluginIO conf maps vars handlers effs =
  runIOStack (runNvimPlugin @r conf maps vars handlers effs)

runNvimPluginIO_ ::
  PluginConfig ->
  Map MappingIdent (PluginHandler '[]) ->
  Map WatchedVariable (Object -> PluginHandler '[]) ->
  [RpcHandler PluginIOStack] ->
  IO ()
runNvimPluginIO_ conf maps vars handlers =
  runIOStack (runNvimPlugin_ conf maps vars handlers)
