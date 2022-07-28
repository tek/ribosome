module Ribosome.Host.Interpreter.Handlers where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)

import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Data.Request (RpcMethod)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (
  Handler,
  RpcHandler (RpcHandler),
  RpcHandlerFun,
  hoistRpcHandlers,
  rpcHandlerMethod,
  )
import qualified Ribosome.Host.Effect.Handlers as Handlers
import Ribosome.Host.Effect.Handlers (Handlers (Register, Run))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.RegisterHandlers (registerHandlers)

interpretHandlersNull :: InterpreterFor (Handlers !! Report) r
interpretHandlersNull =
  interpretResumable \case
    Register ->
      unit
    Run _ _ ->
      pure Nothing

noHandlers :: InterpreterFor (Handlers !! Report) r
noHandlers =
  interpretHandlersNull

handlersByName ::
  [RpcHandler r] ->
  Map RpcMethod (RpcHandlerFun r)
handlersByName =
  Map.fromList . fmap \ rpcDef@(RpcHandler _ _ _ handler) -> (rpcHandlerMethod rpcDef, handler)

runHandler ::
  Map RpcMethod (RpcHandlerFun r) ->
  RpcMethod ->
  [Object] ->
  Handler r (Maybe Object)
runHandler handlers method args =
  traverse ($ args) (Map.lookup method handlers)

-- |Add a set of 'RpcHandlers's to the plugin.
--
-- This can be used multiple times and has to be terminated by 'interpretHandlersNull', which is done automatically when
-- using the plugin main functions.
withHandlers ::
  Members [Handlers !! Report, Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  Sem r a ->
  Sem r a
withHandlers handlersList@(handlersByName -> handlers) =
  interceptResumable \case
    Register -> do
      restop @Report Handlers.register
      registerHandlers handlersList
    Run method args ->
      maybe (runHandler handlers method args) (pure . Just) =<< restop (Handlers.run method args)

interpretHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpreterFor (Handlers !! Report) r
interpretHandlers handlers =
  interpretHandlersNull .
  withHandlers (hoistRpcHandlers raiseUnder handlers)
