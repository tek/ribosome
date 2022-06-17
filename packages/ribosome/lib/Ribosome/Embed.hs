module Ribosome.Embed where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HostConfig (LogConfig)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedExtra, interpretEmbedExtra)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack, TestEffects)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers, sendNvimPlugin)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type HandlerDeps =
  PluginEffects ++ RpcStack ++ EmbedExtra ++ RpcDeps

type TestPluginEmbedStack =
  TestEffects ++ NvimPlugin : HandlerDeps

interpretRpcDeps ::
  Members [Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealNvimEmbed Nothing Nothing

interpretPluginEmbed ::
  Members [Log, Reader LogConfig, Reader PluginName] r =>
  Members IOStack r =>
  InterpretersFor HandlerDeps r
interpretPluginEmbed =
  interpretRpcDeps .
  interpretEmbedExtra .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

withPluginEmbed ::
  Members BasicPluginStack r =>
  Members HandlerDeps r =>
  Member NvimPlugin r =>
  Sem r a ->
  Sem r a
withPluginEmbed =
  sendNvimPlugin .
  interpretBuiltinHandlers .
  interceptHandlersBuiltin .
  withHost .
  insertAt @0

testPluginEmbed ::
  Members BasicPluginStack r =>
  Members HandlerDeps r =>
  Member NvimPlugin r =>
  InterpretersFor TestEffects r
testPluginEmbed =
  withPluginEmbed .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  insertAt @3

embedNvimPluginWith ::
  Members BasicPluginStack r =>
  InterpreterFor NvimPlugin (HandlerDeps ++ r) ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPluginWith handler =
  interpretPluginEmbed .
  handler .
  testPluginEmbed

embedNvimPlugin ::
  Members BasicPluginStack r =>
  [RpcHandler (HandlerDeps ++ r)] ->
  InterpretersFor TestPluginEmbedStack r
embedNvimPlugin handlers =
  embedNvimPluginWith (rpcHandlers handlers)

embedNvimPlugin_ ::
  Members BasicPluginStack r =>
  InterpretersFor TestPluginEmbedStack r
embedNvimPlugin_ =
  embedNvimPlugin []
