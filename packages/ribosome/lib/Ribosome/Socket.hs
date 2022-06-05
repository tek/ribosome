module Ribosome.Socket where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.IOStack (BasicStack)
import Ribosome.Host.Interpreter.Handlers (interceptHandlers, interpretHandlers)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (builtinHandlers)
import Ribosome.Run (PluginEffects, interpretPluginEffects)

type BasicPluginSocketStack =
  RpcStack ++ RpcDeps ++ '[Reader PluginName]

type HandlerStack =
  PluginEffects ++ BasicPluginSocketStack

type PluginSocketStack =
  BuiltinHandlers !! HandlerError : HandlerStack

type PluginHandler r =
  Handler (HandlerStack ++ r) ()

type TestEffects =
  [
    Scratch,
    Settings,
    Rpc
  ]

type TestPluginSocketStack =
  TestEffects ++ PluginSocketStack

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
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor PluginSocketStack r
interpretPluginSocket name maps vars =
  runReader name .
  interpretRpcDeps .
  interpretRpcStack .
  interpretPluginEffects maps vars

withPluginSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  InterpreterFor (Handlers !! HandlerError) (PluginSocketStack ++ r) ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor PluginSocketStack r
withPluginSocket handlers name maps vars =
  interpretPluginSocket name maps vars .
  handlers .
  interceptHandlers (builtinHandlers name) .
  withHost .
  insertAt @0

testPluginSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  InterpreterFor (Handlers !! HandlerError) (PluginSocketStack ++ r) ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor TestPluginSocketStack r
testPluginSocket handlers name maps vars =
  withPluginSocket handlers name maps vars .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  insertAt @3

socketNvimPlugin ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  [RpcHandler (PluginSocketStack ++ r)] ->
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor TestPluginSocketStack r
socketNvimPlugin handlers =
  testPluginSocket (interpretHandlers handlers)

socketNvimPlugin_ ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  PluginName ->
  Map MappingIdent (PluginHandler r) ->
  Map WatchedVariable (Object -> PluginHandler r) ->
  InterpretersFor TestPluginSocketStack r
socketNvimPlugin_ =
  socketNvimPlugin []
