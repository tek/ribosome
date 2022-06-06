module Ribosome.Interpreter.NvimPlugin where

import Data.MessagePack (Object)
import Polysemy.Bundle (runBundle, sendBundle)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.MappingHandler (MappingHandler)
import Ribosome.Effect.NvimPlugin (NvimPlugin (NvimPlugin), NvimPluginEffects, unNvimPlugin)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import Ribosome.Interpreter.MappingHandler (interpretMappingHandler, interpretMappingHandlerNull)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcher, interpretVariableWatcherNull)

pluginHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor NvimPluginEffects r
pluginHandlers handlers maps vars =
  interpretMappingHandler maps .
  interpretVariableWatcher ((raiseUnder .) <$> vars) .
  interpretHandlers (hoistRpcHandlers raiseUnder2 handlers)

interpretNvimPlugin ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpreterFor NvimPlugin r
interpretNvimPlugin handlers maps vars =
  pluginHandlers handlers maps vars .
  runBundle @NvimPluginEffects .
  rewrite unNvimPlugin

rpcHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpreterFor NvimPlugin r
rpcHandlers handlers =
  interpretMappingHandlerNull .
  interpretVariableWatcherNull .
  interpretHandlers (hoistRpcHandlers raiseUnder2 handlers) .
  runBundle @NvimPluginEffects .
  rewrite unNvimPlugin

noHandlers ::
  InterpreterFor NvimPlugin r
noHandlers =
  interpretMappingHandlerNull .
  interpretVariableWatcherNull .
  interpretHandlersNull .
  runBundle @NvimPluginEffects .
  rewrite unNvimPlugin

sendNvimPlugin ::
  Member NvimPlugin r =>
  InterpretersFor NvimPluginEffects r
sendNvimPlugin =
  transform NvimPlugin .
  sendBundle @(MappingHandler !! _) .
  sendBundle @(VariableWatcher !! _) .
  sendBundle @(Handlers !! _) .
  insertAt @3
