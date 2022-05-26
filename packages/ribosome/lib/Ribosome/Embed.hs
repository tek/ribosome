module Ribosome.Embed where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic, interpretSyncAs)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)

import Ribosome.Data.Locks (WatcherLock (WatcherLock))
import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, interpretHostStack)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (testHost)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)

type PluginEffects =
  [
    Reader PluginName,
    AtomicState (Map Text Scratch),
    AtomicState (Map WatchedVariable Object),
    Sync WatcherLock
  ]

type PluginStack =
  Settings !! SettingError : EmbedStack ++ PluginEffects

type PluginHandler r =
  Handler (PluginStack ++ r) ()

interpretPluginEffects ::
  Members [Race, Embed IO] r =>
  PluginName ->
  InterpretersFor PluginEffects r
interpretPluginEffects name =
  interpretSyncAs WatcherLock .
  interpretAtomic mempty .
  interpretAtomic mempty .
  runReader name

embedNvimPluginLog ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPluginLog level name maps vars handlers =
  interpretPluginEffects name .
  interpretLogStdoutLevelConc (Just level) .
  interpretUserErrorPrefixed .
  interpretHostStack .
  interpretSettingsRpc .
  interpretHandlers (builtinHandlers name maps vars <> handlers) .
  testHost .
  insertAt @1

embedNvimPlugin ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin =
  embedNvimPluginLog Warn

embedNvimPlugin_ ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
