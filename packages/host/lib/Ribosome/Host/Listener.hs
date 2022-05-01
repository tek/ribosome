module Ribosome.Host.Listener where

import Data.MessagePack (Object)
import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.Request (RequestId, TrackedRequest (TrackedRequest))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (TrackedResponse (TrackedResponse))
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import qualified Ribosome.Host.Effect.RequestHandler as RequestHandler
import Ribosome.Host.Effect.RequestHandler (RequestHandler)
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)

-- TODO change Process to use Tracked*
-- TODO can TrackedRequest be deparameterized, using Object for the Response field?
handleRequest ::
  Members [RequestHandler !! HandlerError, Process Object (Either Text Object)] r =>
  TrackedRequest Object ->
  Sem r ()
handleRequest (TrackedRequest i req) = do
  response <- (Response.Success <$> RequestHandler.handle req) !! \ (HandlerError e) ->
    pure (Response.Error (RpcError e))
  Process.send (toMsgpack (RpcMessage.Response (TrackedResponse i response)))

sendToConsumer ::
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Log] r =>
  RequestId ->
  Either RpcError Object ->
  Sem r ()
sendToConsumer i msg =
  Responses.respond i msg !! \ (RpcError e) -> Log.error e

-- TODO use Conc.Events for UI events
dispatch ::
  Members [RequestHandler !! HandlerError, Process Object (Either Text Object)] r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Log, Async] r =>
  Object ->
  Sem r ()
dispatch =
  fromMsgpack >>> \case
    Right (RpcMessage.Request req) ->
      void (async (handleRequest req))
    Right (RpcMessage.Response (TrackedResponse i (Response.Success response))) ->
      sendToConsumer i (Right response)
    Right (RpcMessage.Response (TrackedResponse i (Response.Error e))) ->
      sendToConsumer i (Left e)
    Right (RpcMessage.Notification _ payload) ->
      Log.error [exon|Notification not implemented: #{show payload}|]
    Left e ->
      Log.error [exon|Unexpected rpc message format: #{e}|]

listener ::
  Members [RequestHandler !! HandlerError, Process Object (Either Text Object)] r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Log, Async] r =>
  Sem r ()
listener =
  forever do
    Process.recv >>= \case
      Right msg -> do
        Log.debug [exon|listen: #{show msg}|]
        dispatch msg
      Left err ->
        Log.error [exon|listen error: #{err}|]
