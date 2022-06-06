module Ribosome.Socket where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
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
  PluginEffects ++ RpcStack ++ RpcDeps

type TestPluginSocketStack =
  TestEffects ++ NvimPlugin : HandlerDeps

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
  InterpretersFor HandlerDeps r
interpretPluginSocket =
  interpretRpcDeps .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

withPluginSocket ::
  Members BasicPluginStack r =>
  Members HandlerDeps r =>
  Member NvimPlugin r =>
  Sem r a ->
  Sem r a
withPluginSocket =
  sendNvimPlugin .
  interpretBuiltinHandlers .
  interceptHandlersBuiltin .
  withHost .
  insertAt @0

testPluginSocket ::
  Members BasicPluginStack r =>
  Members HandlerDeps r =>
  Member NvimPlugin r =>
  InterpretersFor TestEffects r
testPluginSocket =
  withPluginSocket .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  insertAt @3

socketNvimPluginWith ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  InterpreterFor NvimPlugin (HandlerDeps ++ r) ->
  InterpretersFor TestPluginSocketStack r
socketNvimPluginWith handlers =
  interpretPluginSocket .
  handlers .
  testPluginSocket

socketNvimPlugin ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  [RpcHandler (HandlerDeps ++ r)] ->
  InterpretersFor TestPluginSocketStack r
socketNvimPlugin handlers =
  socketNvimPluginWith (rpcHandlers handlers)

socketNvimPlugin_ ::
  Members BasicPluginStack r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor TestPluginSocketStack r
socketNvimPlugin_ =
  socketNvimPlugin []
