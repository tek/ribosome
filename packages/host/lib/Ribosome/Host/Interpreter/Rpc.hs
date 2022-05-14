module Ribosome.Host.Interpreter.Rpc where

import Data.MessagePack (Object)
import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Data.Request (Request, RequestId, TrackedRequest (TrackedRequest))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
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
  Request ->
  (Object -> Either Text a) ->
  Sem r a
request req decode = do
  reqId <- restop Responses.add
  Process.send (RpcMessage.Request (TrackedRequest reqId (coerce req)))
  restop (Responses.wait reqId) >>= \case
    Response.Success a ->
      stopEitherWith RpcError (decode a)
    Response.Error e ->
      stop e

handleCall ::
  Member Log r =>
  Text ->
  RpcCall a ->
  (Request -> (Object -> Either Text a) -> Sem r a) ->
  Sem r a
handleCall exec call handle =
  RpcCall.cata call & \case
    Right (req, decode) -> do
      Log.debug [exon|#{exec} rpc: #{show req}|]
      handle req decode
    Left a ->
      pure a

interpretRpcMsgpack ::
  ∀ o r .
  Members [Responses RequestId Response !! RpcError, Process RpcMessage o, Log, Async] r =>
  InterpreterFor (Rpc !! RpcError) r
interpretRpcMsgpack =
  interpretResumableH \case
    Rpc.Sync call ->
      pureT =<< handleCall "sync" call request
    Rpc.Async call use -> do
      void $ async do
        a <- runStop @RpcError (handleCall "async" call request)
        runTSimple (use a)
      unitT
    Rpc.Notify call -> do
      handleCall "notify" (void call) \ req _ ->
        Process.send (RpcMessage.Notification req)
      unitT
