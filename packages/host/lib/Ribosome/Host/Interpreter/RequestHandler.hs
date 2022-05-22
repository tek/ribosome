module Ribosome.Host.Interpreter.RequestHandler where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Conc (withAsync_)
import qualified Polysemy.Log as Log
import Polysemy.Log (Severity (Error))
import Polysemy.Process (Process)
import System.IO.Error (IOError)

import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.Event (Event (Event), EventName (EventName))
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), HandlerError (HandlerError))
import Ribosome.Host.Data.Request (Request (Request), RequestId, RpcMethod (RpcMethod))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), RpcHandlerFun, rpcHandlerMethod)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.Errors as Errors
import Ribosome.Host.Effect.Errors (Errors)
import qualified Ribosome.Host.Effect.RequestHandler as RequestHandler
import Ribosome.Host.Effect.RequestHandler (RequestHandler ())
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError, userError)
import Ribosome.Host.Listener (listener)
import Ribosome.Host.RegisterHandlers (registerHandlers)

handlersByName ::
  [RpcHandler r] ->
  Map RpcMethod (RpcHandlerFun r)
handlersByName =
  Map.fromList . fmap \ rpcDef@(RpcHandler _ _ _ handler) -> (rpcHandlerMethod rpcDef, handler)

invalidMethod ::
  Request ->
  Response
invalidMethod (Request (RpcMethod name) _) =
  Response.Error (RpcError [exon|Invalid method for request: #{name}|])

publishEvent ::
  Member (Events er Event) r =>
  Request ->
  Sem r ()
publishEvent (Request (RpcMethod name) args) =
  publish (Event (EventName name) args)

handlerIOError ::
  Members [Error HandlerError, Final IO] r =>
  Sem r a ->
  Sem r a
handlerIOError =
  fromExceptionSemVia \ (e :: IOError) ->
    HandlerError (ErrorMessage "Internal error" ["Handler exception", show e] Error) (Just "RequestHandler")

echoError ::
  Members [Rpc !! RpcError, UserError, Log] r =>
  Text ->
  Severity ->
  Sem r ()
echoError err severity =
  userError err severity >>= traverse_ \ msg ->
    nvimEcho [toMsgpack @[_] [msg]] True mempty !! \ e' ->
      Log.error [exon|Couldn't echo handler error: #{show e'}|]

executeRequest ::
  Members [Rpc !! RpcError, UserError, Errors, Log, Final IO] r =>
  Bool ->
  [Object] ->
  RpcHandlerFun r ->
  Sem r Response
executeRequest notification args handler =
  runError (handlerIOError (handler args)) >>= \case
    Right a ->
      pure (Response.Success a)
    Left (HandlerError msg@(ErrorMessage e log severity) htag) -> do
      Log.log severity (Text.unlines log)
      Errors.store htag msg
      when notification (echoError e severity)
      pure (Response.Error (RpcError e))

handle ::
  Members [Rpc !! RpcError, UserError, Errors, Log, Final IO] r =>
  Bool ->
  Map RpcMethod (RpcHandlerFun r) ->
  Request ->
  Sem r (Maybe Response)
handle notification handlers (Request method args) =
  traverse (executeRequest notification args) (Map.lookup method handlers)

interpretRequestHandler ::
  Members [Rpc !! RpcError, UserError, Errors, Events er Event, Log, Final IO] r =>
  [RpcHandler r] ->
  InterpreterFor RequestHandler r
interpretRequestHandler (handlersByName -> handlers) =
  interpret \case
    RequestHandler.Request req ->
      fromMaybe (invalidMethod req) <$> handle False handlers req
    RequestHandler.Notification req -> do
      res <- handle True handlers req
      when (isNothing res) (publishEvent req)

withRequestHandler ::
  Member (Process RpcMessage (Either Text RpcMessage)) r =>
  Members [Rpc !! RpcError, UserError, Errors, Events er Event, Responses RequestId Response !! RpcError] r =>
  Members [Log, Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler r] ->
  InterpreterFor RequestHandler r
withRequestHandler handlers sem =
  interpretRequestHandler handlers do
    withAsync_ listener do
      raise (registerHandlers handlers)
      sem

runRequestHandler ::
  Member (Process RpcMessage (Either Text RpcMessage)) r =>
  Members [Rpc !! RpcError, UserError, Errors, Events er Event, Responses RequestId Response !! RpcError] r =>
  Members [Log, Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler r] ->
  Sem r ()
runRequestHandler handlers =
  interpretRequestHandler handlers do
    withAsync_ (raise (registerHandlers handlers)) do
      listener
