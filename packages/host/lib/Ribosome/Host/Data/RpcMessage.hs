module Ribosome.Host.Data.RpcMessage where

import Data.MessagePack (Object (ObjectArray, ObjectNil))
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Array (MsgpackArray (msgpackArray))
import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import qualified Ribosome.Host.Data.Request as Request
import Ribosome.Host.Data.Request (Request, TrackedRequest (TrackedRequest), formatReq, formatTrackedReq)
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (TrackedResponse (TrackedResponse), formatTrackedResponse)
import Ribosome.Host.Data.RpcError (RpcError (RpcError))

decodeError :: Object -> RpcError
decodeError =
  RpcError . \case
    Msgpack e ->
      e
    ObjectArray [_, Msgpack e] ->
      e
    o ->
      show o

pattern ErrorPayload :: RpcError -> Object
pattern ErrorPayload e <- (decodeError -> e)

data RpcMessage =
  Request TrackedRequest
  |
  Response TrackedResponse
  |
  Notification Request
  deriving stock (Eq, Show)

instance MsgpackEncode RpcMessage where
  toMsgpack = \case
    Request (TrackedRequest i (Request.Request method payload)) ->
      msgpackArray (0 :: Int) i method payload
    Response (TrackedResponse i (Response.Success payload)) ->
      msgpackArray (1 :: Int) i () payload
    Response (TrackedResponse i (Response.Error payload)) ->
      msgpackArray (1 :: Int) i payload ()
    Notification (Request.Request method payload) ->
      msgpackArray (2 :: Int) method payload

instance MsgpackDecode RpcMessage where
  fromMsgpack = \case
    ObjectArray [Msgpack (0 :: Int), Msgpack i, Msgpack method, Msgpack payload] ->
      Right (Request (TrackedRequest i (Request.Request method payload)))
    ObjectArray [Msgpack (1 :: Int), Msgpack i, ObjectNil, payload] ->
      Right (Response (TrackedResponse i (Response.Success payload)))
    ObjectArray [Msgpack (1 :: Int), Msgpack i, ErrorPayload e, ObjectNil] ->
      Right (Response (TrackedResponse i (Response.Error e)))
    ObjectArray [Msgpack (2 :: Int), Msgpack method, Msgpack payload] ->
      Right (Notification (Request.Request method payload))
    o ->
      Left [exon|Invalid format for RpcMessage: #{show o}|]

instance Serialize RpcMessage where
  put =
    Serialize.put . toMsgpack
  get =
    either (fail . toString) pure . fromMsgpack =<< Serialize.get

formatRpcMsg :: RpcMessage -> Text
formatRpcMsg = \case
  Request req ->
    [exon|request #{formatTrackedReq req}|]
  Response res ->
    [exon|response #{formatTrackedResponse res}|]
  Notification req ->
    [exon|notification #{formatReq req}|]
