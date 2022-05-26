module Ribosome.Host.Listener where

import Exon (exon)
import qualified Polysemy.Conc as Sync
import Polysemy.Conc (interpretAtomic, interpretEventsChan, interpretSyncAs)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (Process)

import Ribosome.Host.Data.Request (RequestId (unRequestId), TrackedRequest (TrackedRequest))
import Ribosome.Host.Data.Response (Response, TrackedResponse (TrackedResponse), formatResponse)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import Ribosome.Host.Data.RpcMessage (RpcMessage, formatRpcMsg)
import qualified Ribosome.Host.Effect.Host as Host
import Ribosome.Host.Effect.Host (Host)
import qualified Ribosome.Host.Effect.Responses as Responses
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Text (ellipsize)

data ResponseLock =
  ResponseLock
  deriving stock (Eq, Show)

data ResponseSent =
  ResponseSent
  deriving stock (Eq, Show)

readyToSend ::
  Member (AtomicState RequestId) r =>
  RequestId ->
  Sem r Bool
readyToSend i =
  atomicGets \ prev -> prev >= i - 1

-- |Send a response, increment the 'RequestId' tracking the latest sent response, and publish an event that unblocks all
-- waiting responses.
sendResponse ::
  Members [Process RpcMessage a, AtomicState RequestId, Events res ResponseSent, Log] r =>
  RequestId ->
  Response ->
  Sem r ()
sendResponse i response = do
  Log.debug [exon|send response: <#{show (unRequestId i)}> #{formatResponse response}|]
  Process.send (RpcMessage.Response (TrackedResponse i response))
  atomicModify' (max i)
  publish ResponseSent

-- |Check whether the last sent response has a 'RequestId' one smaller than the current response.
-- If true, send the response.
-- This is protected by a mutex to avoid deadlock.
-- Returns whether the response was sent for 'sendWhenReady' to decide whether to recurse.
sendIfReady ::
  Member (Events res ResponseSent) r =>
  Members [Sync ResponseLock, Process RpcMessage a, AtomicState RequestId, Log, Resource] r =>
  RequestId ->
  Response ->
  Sem r Bool
sendIfReady i response =
  Sync.lock ResponseLock do
    ifM (readyToSend i) (True <$ sendResponse i response) (pure False)

-- |Neovim doesn't permit responses to be sent out of order.
-- If multiple requests from Neovim have been sent concurrently (e.g. triggered from rpc calls themselves, since the
-- user can't achieve this through the UI due to it being single-threaded), and the first one runs longer than the rest,
-- the others have to wait for the first response to be sent.
-- Otherwise, Neovim will just terminate the client connection.
--
-- To ensure this, the last sent 'RequestId' is stored and compared to the current response's ID before sending.
-- If the last ID is not @i - 1@, this waits until all previous responses are sent.
-- A new attempt to respond is triggered via 'Events' in 'sendResponse'.
-- This function calls 'subscribe' before doing the initial ID comparison, to avoid the race condition in which the last
-- response is sent at the same time that the call to 'subscribe' is made after comparing the IDs unsuccessfully and the
-- 'ResponseSent' event is therefore missed, causing this to block indefinitely.
sendWhenReady ::
  Members [Events res ResponseSent, EventConsumer res ResponseSent] r =>
  Members [Sync ResponseLock, Process RpcMessage a, AtomicState RequestId, Log, Resource] r =>
  RequestId ->
  Response ->
  Sem r ()
sendWhenReady i response =
  subscribe trySend
  where
    trySend =
      unlessM (sendIfReady i response) do
        ResponseSent <- consume
        trySend

dispatch ::
  Members [AtomicState RequestId, Sync ResponseLock, Events res ResponseSent, EventConsumer res ResponseSent] r =>
  Members [Host, Process RpcMessage a, Responses RequestId Response !! RpcError, Log, Resource, Async] r =>
  RpcMessage ->
  Sem r ()
dispatch = \case
  RpcMessage.Request (TrackedRequest i req) ->
    void (async (sendWhenReady i =<< Host.request req))
  RpcMessage.Response (TrackedResponse i response) ->
    Responses.respond i response !! \ (RpcError e) -> Log.error e
  RpcMessage.Notification req ->
    void (async (Host.notification req))

listener ::
  Members [Host, Process RpcMessage (Either Text RpcMessage)] r =>
  Members [Responses RequestId Response !! RpcError, Log, Resource, Race, Async, Embed IO] r =>
  Sem r ()
listener =
  interpretSyncAs ResponseLock $ interpretEventsChan $ interpretAtomic 0 $ forever do
    Process.recv >>= \case
      Right msg -> do
        Log.debug [exon|listen: #{ellipsize 500 (formatRpcMsg msg)}|]
        dispatch msg
      Left err ->
        Log.error [exon|listen error: #{err}|]
