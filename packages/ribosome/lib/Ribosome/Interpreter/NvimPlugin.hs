module Ribosome.Interpreter.NvimPlugin where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import Ribosome.Interpreter.MappingHandler (interpretMappingHandler, interpretMappingHandlerNull)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcher, interpretVariableWatcherNull)

rpcHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpretersFor NvimPlugin' r
rpcHandlers handlers =
  interpretMappingHandlerNull .
  interpretVariableWatcherNull .
  interpretHandlers (hoistRpcHandlers raiseUnder2 handlers)

noHandlers ::
  InterpretersFor NvimPlugin' r
noHandlers =
  interpretMappingHandlerNull .
  interpretVariableWatcherNull .
  interpretHandlersNull

pluginHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor NvimPlugin' r
pluginHandlers handlers maps vars =
  interpretMappingHandler maps .
  interpretVariableWatcher ((raiseUnder .) <$> vars) .
  interpretHandlers (hoistRpcHandlers raiseUnder2 handlers)
