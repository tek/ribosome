module Ribosome.Embed where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
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
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)
import Ribosome.Run (PluginEffects)

type BasicPluginEmbedStack =
  RpcStack ++ EmbedExtra ++ RpcDeps ++ '[Reader PluginName]

type HandlerDeps =
  PluginEffects ++ BasicPluginEmbedStack

type PluginEmbedStack =
  BuiltinHandlers !! HandlerError : HandlerDeps

type PluginHandler r =
  Handler (Handlers !! HandlerError : HandlerDeps ++ r) ()

type TestEffects =
  [
    Scratch,
    Settings,
    Rpc
  ]

type TestPluginEmbedStack =
  TestEffects ++ Handlers !! HandlerError : HandlerDeps

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

withPluginEmbed ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  Member (Handlers !! HandlerError) r =>
  PluginName ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  Sem r a ->
  Sem r a
withPluginEmbed name maps vars =
  interpretBuiltinHandlers maps vars .
  interceptHandlers (builtinHandlers name) .
  withHost .
  insertAt @0

testPluginEmbed ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  Member (Handlers !! HandlerError) r =>
  PluginName ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor TestEffects r
testPluginEmbed name maps vars =
  withPluginEmbed name maps vars .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  insertAt @3

embedNvimPlugin ::
  Members BasicStack r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  [RpcHandler (HandlerDeps ++ r)] ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPlugin name maps vars handlers =
  interpretPluginEmbed name .
  interpretHandlers handlers .
  testPluginEmbed name maps vars

embedNvimPlugin_ ::
  Members BasicStack r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPlugin_ name maps vars =
  embedNvimPlugin name maps vars []
