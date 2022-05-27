module Ribosome.Embed where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, interpretHostStack)
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
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  Severity ->
  PluginName ->
  InterpretersFor BasicPluginStack r
interpretPluginStack level name =
  interpretAtomic mempty .
  runReader name .
  interpretLogStdoutLevelConc (Just level) .
  interpretUserErrorPrefixed .
  interpretHostStack

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
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPluginLog level name maps vars handlers =
  interpretPluginStack level name .
  testPlugin name maps vars handlers

embedNvimPlugin ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> Handler (HandlerStack ++ r) ()) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin =
  embedNvimPluginLog Warn

embedNvimPlugin_ ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> Handler (HandlerStack ++ r) ()) ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
