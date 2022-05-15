module Ribosome.Embed where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Log (Severity (Warn))

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvimLog)
import Ribosome.Plugin.Builtin (builtinHandlers)

type PluginEffects =
  [
    Reader PluginName,
    AtomicState (Map Text Scratch)
  ]

type PluginStack =
  EmbedStack ++ PluginEffects

runPlugin ::
  Member (Embed IO) r =>
  PluginName ->
  InterpretersFor PluginEffects r
runPlugin name =
  interpretAtomic mempty .
  runReader name

embedNvimPluginLog ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  PluginName ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPluginLog level name handlers =
  runPlugin name .
  embedNvimLog level (builtinHandlers name <> handlers)

embedNvimPlugin ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  [RpcHandler (PluginStack ++ r)] ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin =
  embedNvimPluginLog Warn

embedNvimPlugin_ ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  PluginName ->
  InterpretersFor (Rpc : PluginStack) r
embedNvimPlugin_ name =
  embedNvimPlugin name []
