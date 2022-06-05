module Ribosome.Socket where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.IOStack (BasicStack)
import Ribosome.Host.Interpreter.Handlers (interceptHandlers)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)
import Ribosome.Run (PluginEffects)

type BasicPluginSocketStack =
  RpcStack ++ RpcDeps ++ '[Reader PluginName]

type HandlerDeps =
  PluginEffects ++ BasicPluginSocketStack

type PluginSocketStack =
  BuiltinHandlers !! HandlerError : HandlerDeps

type TestEffects =
  [
    Scratch,
    Settings,
    Rpc
  ]

type TestPluginSocketStack =
  TestEffects ++ NvimPlugin' ++ HandlerDeps

interpretRpcDeps ::
  Members [Reader NvimSocket, Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealSocket def .
  resumeHoistError (BootError . show @Text) .
  raiseUnder

interpretPluginSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  PluginName ->
  InterpretersFor HandlerDeps r
interpretPluginSocket name =
  runReader name .
  interpretRpcDeps .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

withPluginSocket ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  Members NvimPlugin' r =>
  PluginName ->
  Sem r a ->
  Sem r a
withPluginSocket name =
  interpretBuiltinHandlers .
  interceptHandlers (builtinHandlers name) .
  withHost .
  insertAt @0

testPluginSocket ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  Members NvimPlugin' r =>
  PluginName ->
  InterpretersFor TestEffects r
testPluginSocket name =
  withPluginSocket name .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  insertAt @3

socketNvimPluginWith ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  PluginName ->
  InterpretersFor NvimPlugin' (HandlerDeps ++ r) ->
  InterpretersFor TestPluginSocketStack r
socketNvimPluginWith name handlers =
  interpretPluginSocket name .
  handlers .
  testPluginSocket name

socketNvimPlugin ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  PluginName ->
  [RpcHandler (HandlerDeps ++ r)] ->
  InterpretersFor TestPluginSocketStack r
socketNvimPlugin name handlers =
  socketNvimPluginWith name $
    rpcHandlers handlers

socketNvimPlugin_ ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  PluginName ->
  InterpretersFor TestPluginSocketStack r
socketNvimPlugin_ name =
  socketNvimPlugin name []
