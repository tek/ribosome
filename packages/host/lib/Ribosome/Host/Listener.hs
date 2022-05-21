module Ribosome.Host.Listener where

import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Data.Request (RequestId (unRequestId), TrackedRequest (TrackedRequest))
import Ribosome.Host.Data.Response (Response, TrackedResponse (TrackedResponse), formatResponse)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import Ribosome.Host.Data.RpcMessage (RpcMessage, formatRpcMsg)
import qualified Ribosome.Host.Effect.RequestHandler as RequestHandler
import Ribosome.Host.Effect.RequestHandler (RequestHandler)
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Text (ellipsize)

handleRequest ::
  Members [RequestHandler, Process RpcMessage a, Log] r =>
  TrackedRequest ->
  Sem r ()
handleRequest (TrackedRequest i req) = do
  response <- RequestHandler.request req
  Log.debug [exon|listen response: <#{show (unRequestId i)}> #{formatResponse response}|]
  Process.send (RpcMessage.Response (TrackedResponse i response))

dispatch ::
  Members [RequestHandler, Process RpcMessage a, Responses RequestId Response !! RpcError, Log, Async] r =>
  RpcMessage ->
  Sem r ()
dispatch = \case
  RpcMessage.Request req ->
    void (async (handleRequest req))
  RpcMessage.Response (TrackedResponse i response) ->
    Responses.respond i response !! \ (RpcError e) -> Log.error e
  RpcMessage.Notification req ->
    void (async (RequestHandler.notification req))

listener ::
  Members [RequestHandler, Process RpcMessage (Either Text RpcMessage)] r =>
  Members [Responses RequestId Response !! RpcError, Log, Async] r =>
  Sem r ()
listener =
  forever do
    Process.recv >>= \case
      Right msg -> do
        Log.debug [exon|listen: #{ellipsize 500 (formatRpcMsg msg)}|]
        dispatch msg
      Left err ->
        Log.error [exon|listen error: #{err}|]
