module Ribosome.Host.Interpreter.Rpc where

import Data.MessagePack (Object)
import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Class.MonadRpc (rpcRequest)
import Ribosome.Host.Class.Msgpack.Error (DecodeError)
import Ribosome.Host.Data.ChannelId (ChannelId)
import Ribosome.Host.Data.Request (
  Request (Request, method),
  RequestId,
  TrackedRequest (TrackedRequest),
  arguments,
  formatReq,
  formatTrackedReq,
  )
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import qualified Ribosome.Host.Data.RpcBatch as RpcBatch
import Ribosome.Host.Data.RpcBatch (RpcBatch)
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
  (Object -> Either DecodeError a) ->
  Sem r a
request exec req@Request {method, arguments} decode = do
  reqId <- restop Responses.add
  let treq = TrackedRequest reqId (coerce req)
  Log.trace [exon|#{exec} rpc: #{formatTrackedReq treq}|]
  Process.send (RpcMessage.Request treq)
  restop (Responses.wait reqId) >>= \case
    Response.Success a ->
      stopEitherWith RpcError.Decode (decode a)
    Response.Error e ->
      stop (RpcError.Api method arguments e)

handleBatch ::
  ∀ r a .
  (∀ x . Request -> (Object -> Either DecodeError x) -> Sem r x) ->
  RpcBatch a ->
  Sem r a
handleBatch handle =
  spin
  where
    spin :: ∀ x . RpcBatch x -> Sem r x
    spin = \case
      RpcBatch.Pure a ->
        pure a
      RpcBatch.Bind fa f ->
        spin . f =<< spin fa
      RpcBatch.Request req decode ->
        handle req decode

handleCall ::
  ∀ a r .
  RpcCall a ->
  (∀ x . Request -> (Object -> Either DecodeError x) -> Sem r x) ->
  Sem r a
handleCall call handle =
  RpcCall.cata call & \case
    batch ->
      handleBatch handle batch

handleNotify ::
  ∀ a o r .
  Members [Process RpcMessage o, Responses RequestId Response !! RpcError, Log, Stop RpcError] r =>
  RpcCall a ->
  Sem r ()
handleNotify call =
  RpcCall.cata call & \case
    RpcBatch.Request req _ -> do
      Log.trace [exon|notify rpc: #{formatReq req}|]
      Process.send (RpcMessage.Notification req)
    batch ->
      void (handleBatch (request "notification with bind") batch)

fetchChannelId ::
  Member (AtomicState (Maybe ChannelId)) r =>
  Members [Process RpcMessage o, Responses RequestId Response !! RpcError, Log, Stop RpcError] r =>
  Sem r ChannelId
fetchChannelId = do
  (cid, ()) <- handleCall (rpcRequest (Request "nvim_get_api_info" [])) (request "sync")
  cid <$ atomicPut (Just cid)

cachedChannelId ::
  Member (AtomicState (Maybe ChannelId)) r =>
  Members [Process RpcMessage o, Responses RequestId Response !! RpcError, Log, Stop RpcError] r =>
  Sem r ChannelId
cachedChannelId =
  maybe fetchChannelId pure =<< atomicGet

interpretRpc ::
  ∀ o r .
  Member (AtomicState (Maybe ChannelId)) r =>
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
      handleNotify call
      unitT
    Rpc.ChannelId ->
      pureT =<< cachedChannelId
