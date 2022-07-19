module Ribosome.Interpreter.NvimPlugin where

import Data.MessagePack (Object)
import Polysemy.Bundle (runBundle, sendBundle)

import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin (NvimPlugin), NvimPluginEffects, unNvimPlugin)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler, hoistRpcHandlers)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcher, interpretVariableWatcherNull)

-- |Run 'NvimPluginEffects' by specifying a set of request handlers, mapping handlers and watched variables.
pluginHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError, Resource, Race, Mask mres, Embed IO] r =>
  [RpcHandler r] ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor NvimPluginEffects r
pluginHandlers handlers vars =
  interpretVariableWatcher vars .
  interpretHandlers (hoistRpcHandlers raiseUnder handlers)

-- |Run the effect bundle 'NvimPlugin' by specifying a set of request handlers, mapping handlers and watched variables.
-- This should be used in combination with 'runNvimPluginIO'.
interpretNvimPlugin ::
  Members [Rpc !! RpcError, Log, Error BootError, Resource, Race, Mask mres, Embed IO] r =>
  [RpcHandler r] ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpreterFor NvimPlugin r
interpretNvimPlugin handlers vars =
  pluginHandlers handlers vars .
  runBundle @NvimPluginEffects .
  rewrite unNvimPlugin

-- |Run the effect bundle 'NvimPlugin' by specifying only a set of request handlers.
rpcHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpreterFor NvimPlugin r
rpcHandlers handlers =
  interpretVariableWatcherNull .
  interpretHandlers (hoistRpcHandlers raiseUnder handlers) .
  runBundle @NvimPluginEffects .
  rewrite unNvimPlugin

-- |Run the effect bundle 'NvimPlugin' without any handlers.
noHandlers ::
  InterpreterFor NvimPlugin r
noHandlers =
  interpretVariableWatcherNull .
  interpretHandlersNull .
  runBundle @NvimPluginEffects .
  rewrite unNvimPlugin

-- |Interpret 'NvimPluginEffects' by converting them into an 'NvimPlugin' bundle.
sendNvimPlugin ::
  Member NvimPlugin r =>
  InterpretersFor NvimPluginEffects r
sendNvimPlugin =
  transform NvimPlugin .
  sendBundle @(VariableWatcher !! _) .
  sendBundle @(Handlers !! _) .
  insertAt @2
