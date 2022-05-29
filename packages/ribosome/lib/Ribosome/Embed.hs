module Ribosome.Embed where

import Data.MessagePack (Object)
import Polysemy.Chronos (ChronosTime)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, interpretCoreDeps, interpretHostEmbedCore)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (testHost)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)

type PluginDeps =
  '[Reader PluginName]

type BasicPluginStack =
  EmbedStack ++ PluginDeps

type PluginEffects =
  [
    Scratch !! RpcError,
    Settings !! SettingError
  ]

type HandlerStack =
  PluginEffects ++ EmbedStack ++ PluginDeps

type PluginStack =
  HandlerStack

type PluginHandler r =
  Handler (HandlerStack ++ r) ()

interpretPluginStack ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  PluginName ->
  InterpretersFor BasicPluginStack r
interpretPluginStack conf name =
  runReader name .
  interpretCoreDeps conf .
  interpretUserErrorPrefixed .
  interpretHostEmbedCore Nothing Nothing

interpretPluginEffects ::
  Members [Rpc !! RpcError, Reader PluginName, Log, Resource, Embed IO] r =>
  InterpretersFor PluginEffects r
interpretPluginEffects =
  interpretSettingsRpc .
  interpretScratch

testPlugin ::
  ∀ r r' .
  r' ~ PluginEffects ++ r =>
  Members BasicPluginStack r =>
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (Handler r' ()) ->
  Map WatchedVariable (Object -> Handler r' ()) ->
  [RpcHandler r'] ->
  InterpretersFor [Rpc, Scratch !! RpcError, Settings !! SettingError] r
testPlugin name maps vars handlers =
  interpretPluginEffects .
  interpretBuiltinHandlers maps vars .
  interpretHandlers (builtinHandlers name <> hoistRpcHandlers raiseUnder handlers) .
  testHost .
  insertAt @1

embedNvimPluginConf ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  HostConfig ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPluginConf conf name maps vars handlers =
  interpretPluginStack conf name .
  testPlugin name maps vars handlers

embedNvimPlugin ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin =
  embedNvimPluginConf def

embedNvimPlugin_ ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
