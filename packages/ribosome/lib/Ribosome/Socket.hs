module Ribosome.Socket where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers, sendNvimPlugin)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type SocketHandlerEffects =
  PluginEffects ++ RpcStack ++ RpcDeps

type PluginSocketStack =
  NvimPlugin : SocketHandlerEffects

interpretRpcDeps ::
  Members [Reader NvimSocket, Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealSocket def .
  resumeHoistError (BootError . show @Text) .
  raiseUnder

interpretPluginSocket ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor SocketHandlerEffects r
interpretPluginSocket =
  interpretRpcDeps .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

withPluginSocket ::
  Members BasicPluginStack r =>
  Members SocketHandlerEffects r =>
  Member NvimPlugin r =>
  Sem r a ->
  Sem r a
withPluginSocket =
  sendNvimPlugin .
  interceptHandlersBuiltin .
  withHost .
  insertAt @0

socketNvimPluginWith ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  InterpreterFor NvimPlugin (SocketHandlerEffects ++ r) ->
  InterpretersFor PluginSocketStack r
socketNvimPluginWith handlers =
  interpretPluginSocket .
  handlers .
  withPluginSocket

socketNvimPlugin ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  [RpcHandler (SocketHandlerEffects ++ r)] ->
  InterpretersFor PluginSocketStack r
socketNvimPlugin handlers =
  socketNvimPluginWith (rpcHandlers handlers)

socketNvimPlugin_ ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor PluginSocketStack r
socketNvimPlugin_ =
  socketNvimPlugin []
