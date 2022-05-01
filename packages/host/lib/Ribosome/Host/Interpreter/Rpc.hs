module Ribosome.Host.Interpreter.Rpc where

import Data.MessagePack (Object (ObjectArray, ObjectInt, ObjectNil, ObjectString))
import Exon (exon)
import Polysemy.Internal.Tactics (liftT)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import qualified Ribosome.Host.Data.Request as Request
import Ribosome.Host.Data.Request (Request (Request), RequestId (RequestId), unRpcMethod)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

nvimRequest :: RequestId -> Request a -> Object
nvimRequest (RequestId reqId) Request {..} =
  ObjectArray [ObjectInt 0, ObjectInt reqId, ObjectString (encodeUtf8 (unRpcMethod method)), ObjectArray payload]

nvimResponse :: RequestId -> Either Object Object -> Object
nvimResponse (RequestId reqId) = \case
  Left err ->
    ObjectArray [ObjectInt 1, ObjectInt reqId, err, ObjectNil]
  Right payload ->
    ObjectArray [ObjectInt 1, ObjectInt reqId, ObjectNil, payload]

nvimNotification :: Request a -> Object
nvimNotification Request {..} =
  ObjectArray [ObjectInt 2, ObjectString (encodeUtf8 (unRpcMethod method)), ObjectArray payload]

request' ::
  MsgpackDecode a =>
  Member (Process Object (Either Text Object)) r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Stop RpcError] r =>
  Request a ->
  Sem r a
request' req = do
  reqId <- restop Responses.add
  reqId <$ Process.send (nvimRequest reqId req)
  raw <- stopEither =<< restop (Responses.wait reqId)
  stopEitherWith RpcError (fromMsgpack raw)

interpretRpcMsgpack ::
  Member (Process Object (Either Text Object)) r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Log, Async] r =>
  InterpreterFor (Rpc !! RpcError) r
interpretRpcMsgpack =
  interpretResumableH \case
    Rpc.Sync req -> do
      Log.debug [exon|sync rpc: #{show req}|]
      liftT (request' req)
    Rpc.Async req use -> do
      Log.debug [exon|async rpc: #{show req}|]
      async do
        resp <- runStop @RpcError (request' req)
        runTSimple (use resp)
      unitT
    Rpc.Notify req -> do
      Log.debug [exon|notify rpc: #{show req}|]
      liftT do
        Process.send (nvimNotification req)
