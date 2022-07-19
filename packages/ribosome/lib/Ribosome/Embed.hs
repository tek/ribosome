module Ribosome.Embed where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HostConfig (LogConfig)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Embed (EmbedExtra, interpretEmbedExtra)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type HandlerEffects =
  PluginEffects ++ RpcStack ++ EmbedExtra ++ RpcDeps

type PluginEmbedStack =
  NvimPlugin ++ HandlerEffects

interpretRpcDeps ::
  Members [Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealNvimEmbed Nothing Nothing

interpretPluginEmbed ::
  Members [Log, Reader LogConfig, Reader PluginName] r =>
  Members IOStack r =>
  InterpretersFor HandlerEffects r
interpretPluginEmbed =
  interpretRpcDeps .
  interpretEmbedExtra .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

withPluginEmbed ::
  Members BasicPluginStack r =>
  Members HandlerEffects r =>
  Members NvimPlugin r =>
  Sem r a ->
  Sem r a
withPluginEmbed =
  interceptHandlersBuiltin .
  withHost .
  insertAt @0

embedNvimPluginWith ::
  Members BasicPluginStack r =>
  InterpretersFor NvimPlugin (HandlerEffects ++ r) ->
  InterpretersFor PluginEmbedStack r
embedNvimPluginWith handlers =
  interpretPluginEmbed .
  handlers .
  withPluginEmbed

embedNvimPlugin ::
  Members BasicPluginStack r =>
  [RpcHandler (HandlerEffects ++ r)] ->
  InterpretersFor PluginEmbedStack r
embedNvimPlugin handlers =
  embedNvimPluginWith (rpcHandlers handlers)

embedNvimPlugin_ ::
  Members BasicPluginStack r =>
  InterpretersFor PluginEmbedStack r
embedNvimPlugin_ =
  embedNvimPlugin []
