module Ribosome.Embed where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (LogConfig)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Embed (EmbedExtra, interpretEmbedExtra)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type HandlerEffects =
  PluginEffects ++ RpcStack ++ EmbedExtra ++ RpcDeps

type PluginEmbedStack =
  Handlers !! HandlerError : HandlerEffects

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
  interpretVariableWatcherNull .
  interpretSettingsRpc .
  interpretScratch

withPluginEmbed ::
  Members BasicPluginStack r =>
  Members HandlerEffects r =>
  Member (Handlers !! HandlerError) r =>
  Sem r a ->
  Sem r a
withPluginEmbed =
  interceptHandlersBuiltin .
  withHost .
  insertAt @0

embedNvimPlugin ::
  Members BasicPluginStack r =>
  [RpcHandler (HandlerEffects ++ r)] ->
  InterpretersFor PluginEmbedStack r
embedNvimPlugin handlers =
  interpretPluginEmbed .
  interpretHandlers handlers .
  withPluginEmbed

embedNvimPlugin_ ::
  Members BasicPluginStack r =>
  InterpretersFor PluginEmbedStack r
embedNvimPlugin_ =
  embedNvimPlugin []
