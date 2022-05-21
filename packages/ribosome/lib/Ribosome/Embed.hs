module Ribosome.Embed where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic, interpretSyncAs)
import Polysemy.Log (Severity (Warn))

import Ribosome.Data.Locks (WatcherLock (WatcherLock))
import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvimLog)
import Ribosome.Plugin.Builtin (builtinHandlers)

type PluginEffects =
  [
    Reader PluginName,
    AtomicState (Map Text Scratch),
    AtomicState (Map WatchedVariable Object),
    Sync WatcherLock
  ]

type PluginStack =
  EmbedStack ++ PluginEffects

type PluginHandler r =
  Handler (PluginStack ++ r) ()

runPlugin ::
  Members [Race, Embed IO] r =>
  PluginName ->
  InterpretersFor PluginEffects r
runPlugin name =
  interpretSyncAs WatcherLock .
  interpretAtomic mempty .
  interpretAtomic mempty .
  runReader name

embedNvimPluginLog ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPluginLog level name maps vars handlers =
  runPlugin name .
  embedNvimLog level (builtinHandlers name maps vars <> handlers)

embedNvimPlugin ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin =
  embedNvimPluginLog Warn

embedNvimPlugin_ ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
