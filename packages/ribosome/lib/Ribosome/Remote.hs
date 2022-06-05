module Ribosome.Remote where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.IOStack (BasicStack, IOStack, runBasicStack)
import Ribosome.Host.Interpreter.Handlers (interceptHandlers, interpretHandlers)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)
import Ribosome.Run (PluginEffects, interpretPluginEffects)

type BasicPluginRemoteStack =
  RpcStack ++ RpcDeps ++ '[Reader PluginName]

type PluginRemoteStack =
  BuiltinHandlers !! HandlerError : PluginEffects ++ BasicPluginRemoteStack

type PluginRemoteIOStack =
  PluginRemoteStack ++ BasicStack

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
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor PluginRemoteStack r
interpretPluginRemote name maps vars =
  runReader name .
  interpretRpcDeps .
  interpretRpcStack .
  interpretPluginEffects maps vars

-- TODO
-- create effects for mapping and variable handlers
-- run BuiltinHandlers directly after interceptHandlers here
-- remove interpretPluginRemote, effs and handlers here
-- offer two workflows:
-- * run interpretPluginRemote . interpretersForMapsVarsAndRpc . runPluginRemote
-- * runNvimPlugin handlers
-- no others that involve passing in handlers for vars and maps as Maps
runPluginRemote ::
  ∀ r .
  Members PluginRemoteIOStack (r ++ PluginRemoteIOStack) =>
  InterpreterFor (Handlers !! HandlerError) (r ++ PluginRemoteIOStack) ->
  InterpretersFor r PluginRemoteIOStack ->
  PluginName ->
  Map MappingIdent (PluginHandler BasicStack) ->
  Map WatchedVariable (Object -> PluginHandler BasicStack) ->
  Sem BasicStack ()
runPluginRemote handlers effs name maps vars =
  interpretPluginRemote name maps vars $
  effs $
  handlers $
  interceptHandlers (builtinHandlers name) runHost

runNvimPlugin ::
  ∀ r .
  Members PluginRemoteIOStack (r ++ PluginRemoteIOStack) =>
  [RpcHandler (r ++ PluginRemoteIOStack)] ->
  InterpretersFor r PluginRemoteIOStack ->
  PluginName ->
  Map MappingIdent (PluginHandler BasicStack) ->
  Map WatchedVariable (Object -> PluginHandler BasicStack) ->
  Sem BasicStack ()
runNvimPlugin handlers =
  runPluginRemote @r (interpretHandlers handlers)

runNvimPlugin_ ::
  [RpcHandler PluginRemoteIOStack] ->
  PluginName ->
  Map MappingIdent (PluginHandler BasicStack) ->
  Map WatchedVariable (Object -> PluginHandler BasicStack) ->
  Sem BasicStack ()
runNvimPlugin_ handlers =
  runPluginRemote @'[] (interpretHandlers handlers) id

runNvimPluginIO ::
  ∀ r .
  Members PluginRemoteIOStack (r ++ PluginRemoteIOStack) =>
  PluginConfig ->
  [RpcHandler (r ++ PluginRemoteIOStack)] ->
  InterpretersFor r PluginRemoteIOStack ->
  Map MappingIdent (PluginHandler BasicStack) ->
  Map WatchedVariable (Object -> PluginHandler BasicStack) ->
  IO ()
runNvimPluginIO (PluginConfig name conf) handlers effs maps vars =
  runBasicStack conf (runNvimPlugin @r handlers effs name maps vars)

runNvimPluginIO_ ::
  PluginConfig ->
  [RpcHandler PluginRemoteIOStack] ->
  Map MappingIdent (PluginHandler BasicStack) ->
  Map WatchedVariable (Object -> PluginHandler BasicStack) ->
  IO ()
runNvimPluginIO_ (PluginConfig name conf) handlers maps vars =
  runBasicStack conf (runNvimPlugin_ handlers name maps vars)
