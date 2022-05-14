module Ribosome.Host.Listener where

import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Data.Request (RequestId, Request, TrackedRequest (TrackedRequest))
import Ribosome.Host.Data.Response (Response, TrackedResponse (TrackedResponse))
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.RequestHandler as RequestHandler
import Ribosome.Host.Effect.RequestHandler (RequestHandler)
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Text (ellipsize)

handleRequest ::
  Members [RequestHandler, Process RpcMessage a] r =>
  TrackedRequest ->
  Sem r ()
handleRequest (TrackedRequest i req) = do
  response <- RequestHandler.request req
  Process.send (RpcMessage.Response (TrackedResponse i response))

handleNotification ::
  Members [RequestHandler, Log] r =>
  Request ->
  Sem r ()
handleNotification req = do
  RequestHandler.notification req

sendToConsumer ::
  Members [Responses RequestId Response !! RpcError, Log] r =>
  RequestId ->
  Response ->
  Sem r ()
sendToConsumer i msg =
  Responses.respond i msg !! \ (RpcError e) -> Log.error e

dispatch ::
  Members [RequestHandler, Process RpcMessage a, Responses RequestId Response !! RpcError, Log, Async] r =>
  RpcMessage ->
  Sem r ()
dispatch = \case
  RpcMessage.Request req ->
    void (async (handleRequest req))
  RpcMessage.Response (TrackedResponse i response) ->
    sendToConsumer i response
  RpcMessage.Notification req ->
    void (async (handleNotification req))

listener ::
  Members [RequestHandler, Process RpcMessage (Either Text RpcMessage)] r =>
  Members [Responses RequestId Response !! RpcError, Log, Async] r =>
  Sem r ()
listener =
  forever do
    Process.recv >>= \case
      Right msg -> do
        Log.debug [exon|listen: #{ellipsize 500 (show msg)}|]
        dispatch msg
      Left err ->
        Log.error [exon|listen error: #{err}|]
