module Ribosome.Host.Interpreter.RequestHandler where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Exon (exon)
import Polysemy.Conc (withAsync_)
import Polysemy.Process (Process)

import Ribosome.Host.Data.Event (Event (Event), EventName (EventName))
import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.Request (Request (Request), RequestId, RpcMethod (RpcMethod), SomeRequest)
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), RpcHandlerFun, rpcHandlerMethod)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.RequestHandler as RequestHandler
import Ribosome.Host.Effect.RequestHandler (RequestHandler ())
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
  SomeRequest ->
  Response
invalidMethod (Request (RpcMethod name) _) =
  Response.Error (RpcError [exon|Invalid method for request: #{name}|])

publishEvent ::
  Member (Events er Event) r =>
  SomeRequest ->
  Sem r ()
publishEvent (Request (RpcMethod name) args) =
  publish (Event (EventName name) args)

executeRequest ::
  [Object] ->
  RpcHandlerFun r ->
  Sem r Response
executeRequest args handler =
  runError (handler args) <&> \case
    Right a -> Response.Success a
    Left (HandlerError e) -> Response.Error (RpcError e)

handle ::
  Map RpcMethod (RpcHandlerFun r) ->
  SomeRequest ->
  Sem r (Maybe Response)
handle handlers (Request method args) =
  traverse (executeRequest args) (Map.lookup method handlers)

interpretRequestHandler ::
  Member (Events er Event) r =>
  [RpcHandler r] ->
  InterpreterFor RequestHandler r
interpretRequestHandler (handlersByName -> handlers) =
  interpret \case
    RequestHandler.Request req ->
      fromMaybe (invalidMethod req) <$> handle handlers req
    RequestHandler.Notification req -> do
      res <- handle handlers req
      when (isNothing res) (publishEvent req)

withRequestHandler ::
  Members [Process RpcMessage (Either Text RpcMessage), Rpc !! RpcError, Log, Error Text] r =>
  Members [Events er Event, Responses RequestId Response !! RpcError, Resource, Race, Async] r =>
  [RpcHandler r] ->
  InterpreterFor RequestHandler r
withRequestHandler handlers sem = do
  interpretRequestHandler handlers do
    withAsync_ listener do
      raise (registerHandlers handlers)
      sem

runRequestHandler ::
  Members [Process RpcMessage (Either Text RpcMessage), Rpc !! RpcError, Log, Error Text] r =>
  Members [Events er Event, Responses RequestId Response !! RpcError, Resource, Race, Async] r =>
  [RpcHandler r] ->
  Sem r ()
runRequestHandler handlers = do
  interpretRequestHandler handlers do
    withAsync_ (raise (registerHandlers handlers)) do
      listener
