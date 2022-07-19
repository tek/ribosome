module Ribosome.Interpreter.NvimPlugin where

import Data.MessagePack (Object)

import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcher, interpretVariableWatcherNull)

-- |Run 'NvimPlugin' by specifying a set of request handlers and watched variables.
-- This should be used in combination with 'runNvimPluginIO'.
pluginHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError, Resource, Race, Mask mres, Embed IO] r =>
  [RpcHandler r] ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor NvimPlugin r
pluginHandlers handlers vars =
  interpretVariableWatcher vars .
  interpretHandlers (hoistRpcHandlers raiseUnder handlers)

-- |Run 'NvimPlugin' by specifying only a set of request handlers.
rpcHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpretersFor NvimPlugin r
rpcHandlers handlers =
  interpretVariableWatcherNull .
  interpretHandlers (hoistRpcHandlers raiseUnder handlers)

-- |Run 'NvimPlugin' without any handlers.
noHandlers ::
  InterpretersFor NvimPlugin r
noHandlers =
  interpretVariableWatcherNull .
  interpretHandlersNull
