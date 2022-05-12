module Ribosome.Host.Interpreter.RequestHandler where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Exon (exon)
import Polysemy.Conc (withAsync_)
import Polysemy.Process (Process)

import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.Request (Request (Request), RequestId, RpcMethod (RpcMethod))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), RpcHandlerFun, rpcHandlerMethod)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.RequestHandler (RequestHandler (Handle))
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Listener (listener)
import Ribosome.Host.RegisterHandlers (registerHandlers)

handlersByName ::
  [RpcHandler r] ->
  Map RpcMethod (RpcHandlerFun r)
handlersByName =
  Map.fromList . fmap \ rpcDef@(RpcHandler _ _ _ handler) -> (rpcHandlerMethod rpcDef, handler)

invalidMethod ::
  RpcMethod ->
  Response
invalidMethod (RpcMethod name) =
  Response.Error (RpcError [exon|Invalid method: #{name}|])

executeRequest ::
  [Object] ->
  RpcHandlerFun r ->
  Sem r Response
executeRequest args handle =
  runError (handle args) <&> \case
    Right a -> Response.Success a
    Left (HandlerError e) -> Response.Error (RpcError e)

interpretRequestHandler ::
  [RpcHandler r] ->
  InterpreterFor RequestHandler r
interpretRequestHandler defs =
  interpret \case
    Handle (Request name args) ->
      maybe (pure (invalidMethod name)) (executeRequest args) (Map.lookup name handlers)
  where
    handlers =
      handlersByName defs

withRequestHandler ::
  Members [Process RpcMessage (Either Text RpcMessage), Rpc !! RpcError, Log, Error Text] r =>
  Members [Responses RequestId Response !! RpcError, Resource, Race, Async] r =>
  [RpcHandler r] ->
  InterpreterFor RequestHandler r
withRequestHandler defs sem = do
  interpretRequestHandler defs do
    withAsync_ listener do
      raise (registerHandlers defs)
      sem

runRequestHandler ::
  Members [Process RpcMessage (Either Text RpcMessage), Rpc !! RpcError, Log, Error Text] r =>
  Members [Responses RequestId Response !! RpcError, Resource, Race, Async] r =>
  [RpcHandler r] ->
  Sem r ()
runRequestHandler defs = do
  interpretRequestHandler defs do
    withAsync_ (raise (registerHandlers defs)) do
      listener
