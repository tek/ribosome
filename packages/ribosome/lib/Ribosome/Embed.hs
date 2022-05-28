module Ribosome.Embed where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic)
import Time (GhcTime)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, interpretCoreDeps, interpretHostEmbedCore)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (testHost)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)

type PluginEffects =
  [
    Reader PluginName,
    AtomicState (Map Text Scratch)
  ]

type BasicPluginStack =
  EmbedStack ++ PluginEffects

type HandlerStack =
  Settings !! SettingError :
  EmbedStack ++ PluginEffects

type PluginStack =
  HandlerStack

type PluginHandler r =
  Handler (HandlerStack ++ r) ()

interpretPluginStack ::
  Members [Error BootError, GhcTime, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  PluginName ->
  InterpretersFor BasicPluginStack r
interpretPluginStack conf name =
  interpretAtomic mempty .
  runReader name .
  interpretCoreDeps conf .
  interpretUserErrorPrefixed .
  interpretHostEmbedCore Nothing Nothing

testPlugin ::
  ∀ r r' .
  r' ~ Settings !! SettingError : r =>
  Members BasicPluginStack r =>
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (Handler r' ()) ->
  Map WatchedVariable (Object -> Handler r' ()) ->
  [RpcHandler r'] ->
  InterpretersFor [Rpc, Settings !! SettingError] r
testPlugin name maps vars handlers =
  interpretSettingsRpc .
  interpretBuiltinHandlers maps vars .
  interpretHandlers (builtinHandlers name <> hoistRpcHandlers raiseUnder handlers) .
  testHost .
  insertAt @1

embedNvimPluginLog ::
  Members [Error BootError, GhcTime, Resource, Race, Async, Embed IO, Final IO] r =>
  HostConfig ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPluginLog conf name maps vars handlers =
  interpretPluginStack conf name .
  testPlugin name maps vars handlers

embedNvimPlugin ::
  Members [Error BootError, GhcTime, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin =
  embedNvimPluginLog def

embedNvimPlugin_ ::
  Members [Error BootError, GhcTime, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
