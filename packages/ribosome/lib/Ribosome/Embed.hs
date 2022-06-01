module Ribosome.Embed where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, interpretCoreDeps, interpretHostEmbedCore)
import Ribosome.Host.IOStack (IOStack)
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
  PluginEffects ++ BasicPluginStack

type PluginStack =
  BuiltinHandlers !! HandlerError : HandlerStack

type TestEffects =
  [
    Scratch,
    Settings,
    Rpc
  ]

type TestPluginStack =
  TestEffects ++ PluginStack

type PluginHandler r =
  Handler (HandlerStack ++ r) ()

interpretPluginStack ::
  Members IOStack r =>
  HostConfig ->
  PluginName ->
  InterpretersFor BasicPluginStack r
interpretPluginStack conf name =
  runReader name .
  interpretCoreDeps conf .
  interpretUserErrorPrefixed .
  interpretHostEmbedCore Nothing Nothing

interpretPluginEffects ::
  ∀ r r' .
  r' ~ PluginEffects ++ r =>
  Members IOStack r =>
  Members BasicPluginStack r =>
  Map MappingIdent (Handler r' ()) ->
  Map WatchedVariable (Object -> Handler r' ()) ->
  InterpretersFor (BuiltinHandlers !! HandlerError : PluginEffects) r
interpretPluginEffects maps vars =
  interpretSettingsRpc .
  interpretScratch .
  interpretBuiltinHandlers maps vars

interpretPlugin ::
  Members IOStack r =>
  HostConfig ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor PluginStack r
interpretPlugin conf name maps vars =
  interpretPluginStack conf name .
  interpretPluginEffects maps vars

testPlugin ::
  ∀ r .
  Members IOStack r =>
  Members PluginStack r =>
  PluginName ->
  [RpcHandler r] ->
  InterpretersFor [Scratch, Settings, Rpc] r
testPlugin name handlers =
  interpretHandlers (builtinHandlers name <> handlers) .
  testHost .
  resumeHoistError @_ @Settings (BootError . show @Text) .
  resumeHoistError @_ @Scratch (BootError . show @Text) .
  insertAt @3

embedNvimPluginConf ::
  Members IOStack r =>
  HostConfig ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor TestPluginStack r
embedNvimPluginConf conf name maps vars handlers =
  interpretPlugin conf name maps vars .
  testPlugin name handlers

embedNvimPlugin ::
  Members IOStack r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor TestPluginStack r
embedNvimPlugin =
  embedNvimPluginConf def

embedNvimPlugin_ ::
  Members IOStack r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor TestPluginStack r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
