module Ribosome.Host.Interpreter.Handlers where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)

import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
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

interpretHandlersNull :: InterpreterFor (Handlers !! HandlerError) r
interpretHandlersNull =
  interpretResumable \case
    Register ->
      unit
    Run _ _ ->
      pure Nothing

noHandlers :: InterpreterFor (Handlers !! HandlerError) r
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

interceptHandlers ::
  Members [Handlers !! HandlerError, Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  Sem r a ->
  Sem r a
interceptHandlers handlersList@(handlersByName -> handlers) =
  interceptResumable \case
    Register -> do
      restop @HandlerError Handlers.register
      registerHandlers handlersList
    Run method args ->
      maybe (runHandler handlers method args) (pure . Just) =<< restop (Handlers.run method args)

interpretHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpreterFor (Handlers !! HandlerError) r
interpretHandlers handlers =
  interpretHandlersNull .
  interceptHandlers (hoistRpcHandlers raiseUnder handlers)
