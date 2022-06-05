module Ribosome.Embed where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedExtra, interpretEmbedExtra)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.IOStack (BasicStack, IOStack)
import Ribosome.Host.Interpreter.Handlers (interceptHandlers, interpretHandlers)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.MappingHandler (interpretMappingHandlerNull)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Plugin.Builtin (builtinHandlers)
import Ribosome.Run (PluginEffects)

type BasicPluginEmbedStack =
  RpcStack ++ EmbedExtra ++ RpcDeps ++ '[Reader PluginName]

type HandlerDeps =
  PluginEffects ++ BasicPluginEmbedStack

type PluginEmbedStack =
  BuiltinHandlers !! HandlerError : HandlerDeps

type TestEffects =
  [
    Scratch,
    Settings,
    Rpc
  ]

type TestPluginEmbedStack =
  TestEffects ++ NvimPlugin' ++ HandlerDeps

interpretRpcDeps ::
  Members [Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealNvimEmbed Nothing Nothing

interpretPluginEmbed ::
  Member Log r =>
  Members IOStack r =>
  PluginName ->
  InterpretersFor HandlerDeps r
interpretPluginEmbed name =
  runReader name .
  interpretRpcDeps .
  interpretEmbedExtra .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

-- TODO pointless?
interpretPluginHandlersEmbed ::
  Member Log r =>
  Members IOStack r =>
  PluginName ->
  [RpcHandler (HandlerDeps ++ r)] ->
  InterpretersFor (Handlers !! HandlerError : HandlerDeps) r
interpretPluginHandlersEmbed name handlers =
  interpretPluginEmbed name .
  interpretHandlers handlers

withPluginEmbed ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  Members NvimPlugin' r =>
  PluginName ->
  Sem r a ->
  Sem r a
withPluginEmbed name =
  interpretBuiltinHandlers .
  interceptHandlers (builtinHandlers name) .
  withHost .
  insertAt @0

testPluginEmbed ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  Members NvimPlugin' r =>
  PluginName ->
  InterpretersFor TestEffects r
testPluginEmbed name =
  withPluginEmbed name .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  insertAt @3

embedNvimPluginWith ::
  Members BasicStack r =>
  PluginName ->
  InterpretersFor NvimPlugin' (HandlerDeps ++ r) ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPluginWith name handlers =
  interpretPluginEmbed name .
  handlers .
  testPluginEmbed name

embedNvimPlugin ::
  Members BasicStack r =>
  PluginName ->
  [RpcHandler (HandlerDeps ++ r)] ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPlugin name handlers =
  embedNvimPluginWith name $
    interpretMappingHandlerNull .
    interpretVariableWatcherNull .
    interpretHandlers (hoistRpcHandlers raiseUnder2 handlers)

embedNvimPlugin_ ::
  Members BasicStack r =>
  PluginName ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPlugin_ name =
  embedNvimPlugin name []
