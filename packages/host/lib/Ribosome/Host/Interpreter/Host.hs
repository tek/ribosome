module Ribosome.Host.Interpreter.Host where

import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Conc (withAsync_)
import qualified Polysemy.Log as Log
import Polysemy.Log (Severity (Error))
import Polysemy.Process (Process)
import System.IO.Error (IOError)

import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event (Event), EventName (EventName))
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), HandlerError (HandlerError))
import Ribosome.Host.Data.Request (Request (Request), RequestId, RpcMethod (RpcMethod))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.Errors as Errors
import Ribosome.Host.Effect.Errors (Errors)
import qualified Ribosome.Host.Effect.Handlers as Handlers
import Ribosome.Host.Effect.Handlers (Handlers)
import qualified Ribosome.Host.Effect.Host as Host
import Ribosome.Host.Effect.Host (Host)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError, userError)
import Ribosome.Host.Listener (listener)

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
    HandlerError (ErrorMessage "Internal error" ["Handler exception", show e] Error) (Just "Host")

echoError ::
  Members [Rpc !! RpcError, UserError, Log] r =>
  Text ->
  Severity ->
  Sem r ()
echoError err severity =
  userError err severity >>= traverse_ \ msg ->
    nvimEcho [toMsgpack @[_] [msg]] True mempty !! \ e' ->
      Log.error [exon|Couldn't echo handler error: #{show e'}|]

handle ::
  Members [Handlers !! HandlerError, Rpc !! RpcError, UserError, Errors, Log, Final IO] r =>
  Bool ->
  Request ->
  Sem r (Maybe Response)
handle notification (Request method args) =
  runError (handlerIOError (resuming throw (Handlers.run method args))) >>= \case
    Right Nothing ->
      pure Nothing
    Right (Just a) ->
      pure (Just (Response.Success a))
    Left (HandlerError msg@(ErrorMessage e log severity) htag) -> do
      Log.log severity (Text.unlines log)
      Errors.store htag msg
      when notification (echoError e severity)
      pure (Just (Response.Error (RpcError e)))

interpretHost ::
  Members [Handlers !! HandlerError, Rpc !! RpcError, UserError, Errors, Events er Event, Log, Final IO] r =>
  InterpreterFor Host r
interpretHost =
  interpret \case
    Host.Request req ->
      fromMaybe (invalidMethod req) <$> handle False req
    Host.Notification req -> do
      res <- handle True req
      when (isNothing res) (publishEvent req)

register ::
  Members [Handlers !! HandlerError, Error BootError] r =>
  Sem r ()
register =
  Handlers.register !! \ e -> throw (BootError [exon|Registering handlers: #{show e}|])

type HostStack er =
  [
    Handlers !! HandlerError,
    Process RpcMessage (Either Text RpcMessage),
    Rpc !! RpcError,
    UserError,
    Errors,
    Events er Event,
    Responses RequestId Response !! RpcError,
    Log,
    Error BootError,
    Resource,
    Race,
    Async,
    Embed IO,
    Final IO
  ]

withHost ::
  Members (HostStack er) r =>
  InterpreterFor Host r
withHost sem =
  interpretHost do
    withAsync_ listener do
      register
      sem

testHost ::
  Members (HostStack er) r =>
  InterpretersFor [Rpc, Host] r
testHost =
  withHost .
  resumeHoistError @_ @Rpc (BootError . show @Text)

runHost ::
  Members (HostStack er) r =>
  Sem r ()
runHost =
  interpretHost do
    withAsync_ register do
      listener
