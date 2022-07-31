module Ribosome.Host.Interpreter.Rpc where

import Data.MessagePack (Object)
import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Data.Request (
  Request (Request, method),
  RequestId,
  TrackedRequest (TrackedRequest),
  formatReq,
  formatTrackedReq,
  )
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcCall (RpcCall)
import qualified Ribosome.Host.Data.RpcError as RpcError
import Ribosome.Host.Data.RpcError (RpcError)
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import qualified Ribosome.Host.RpcCall as RpcCall

request ::
  ∀ a o r .
  Members [Process RpcMessage o, Responses RequestId Response !! RpcError, Log, Stop RpcError] r =>
  Text ->
  Request ->
  (Object -> Either Text a) ->
  Sem r a
request exec req@Request {method} decode = do
  reqId <- restop Responses.add
  let treq = TrackedRequest reqId (coerce req)
  Log.trace [exon|#{exec} rpc: #{formatTrackedReq treq}|]
  Process.send (RpcMessage.Request treq)
  restop (Responses.wait reqId) >>= \case
    Response.Success a ->
      stopEitherWith RpcError.Decode (decode a)
    Response.Error e ->
      stop (RpcError.Api method e)

handleCall ::
  RpcCall a ->
  (Request -> (Object -> Either Text a) -> Sem r a) ->
  Sem r a
handleCall call handle =
  RpcCall.cata call & \case
    Right (req, decode) -> do
      handle req decode
    Left a ->
      pure a

interpretRpc ::
  ∀ o r .
  Members [Responses RequestId Response !! RpcError, Process RpcMessage o, Log, Async] r =>
  InterpreterFor (Rpc !! RpcError) r
interpretRpc =
  interpretResumableH \case
    Rpc.Sync call ->
      pureT =<< handleCall call (request "sync")
    Rpc.Async call use -> do
      void $ async do
        a <- runStop @RpcError (handleCall call (request "async"))
        runTSimple (use a)
      unitT
    Rpc.Notify call -> do
      handleCall (void call) \ req _ -> do
        Log.trace [exon|notify rpc: #{formatReq req}|]
        Process.send (RpcMessage.Notification req)
      unitT
