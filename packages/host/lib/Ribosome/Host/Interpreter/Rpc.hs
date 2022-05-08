module Ribosome.Host.Interpreter.Rpc where

import Exon (exon)
import Polysemy.Internal.Tactics (liftT)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import qualified Ribosome.Host.Data.Request as Request
import Ribosome.Host.Data.Request (Request (Request), RequestId, TrackedRequest (TrackedRequest))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

request ::
  ∀ a o r .
  MsgpackDecode a =>
  Members [Process RpcMessage o, Responses RequestId Response !! RpcError, Stop RpcError] r =>
  Request a ->
  Sem r a
request req = do
  reqId <- restop Responses.add
  Process.send (RpcMessage.Request (TrackedRequest reqId (coerce req)))
  restop (Responses.wait reqId) >>= \case
    Response.Success a ->
      stopEitherWith RpcError (fromMsgpack a)
    Response.Error e ->
      stop e

interpretRpcMsgpack ::
  ∀ o r .
  Members [Responses RequestId Response !! RpcError, Process RpcMessage o, Log, Async] r =>
  InterpreterFor (Rpc !! RpcError) r
interpretRpcMsgpack =
  interpretResumableH \case
    Rpc.Sync req -> do
      Log.debug [exon|sync rpc: #{show req}|]
      liftT (request req)
    Rpc.Async req use -> do
      Log.debug [exon|async rpc: #{show req}|]
      async do
        resp <- runStop @RpcError (request req)
        runTSimple (use resp)
      unitT
    Rpc.Notify req@Request {..} -> do
      Log.debug [exon|notify rpc: #{show req}|]
      liftT do
        Process.send (RpcMessage.Notification method payload)
