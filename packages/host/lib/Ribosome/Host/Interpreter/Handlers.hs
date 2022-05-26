module Ribosome.Host.Interpreter.Handlers where

import qualified Data.Map.Strict as Map

import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.Request (RpcMethod)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), RpcHandlerFun, rpcHandlerMethod)
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

handlersByName ::
  [RpcHandler r] ->
  Map RpcMethod (RpcHandlerFun r)
handlersByName =
  Map.fromList . fmap \ rpcDef@(RpcHandler _ _ _ handler) -> (rpcHandlerMethod rpcDef, handler)

interpretHandlers ::
  Members [Rpc !! RpcError, Log, Error BootError] r =>
  [RpcHandler r] ->
  InterpreterFor (Handlers !! HandlerError) r
interpretHandlers handlersList@(handlersByName -> handlers) =
  interpretResumable \case
    Register ->
      registerHandlers handlersList
    Run method args ->
      traverse ($ args) (Map.lookup method handlers)
