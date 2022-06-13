module Ribosome.Host.Data.Request where

import Data.MessagePack (Object (ObjectArray))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

newtype RpcMethod =
  RpcMethod { unRpcMethod :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode, Semigroup, Monoid)

newtype RequestId =
  RequestId { unRequestId :: Int64 }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, MsgpackDecode, MsgpackEncode)

data Request =
  Request {
    method :: RpcMethod,
    payload :: [Object]
  }
  deriving stock (Eq, Show, Generic)

instance MsgpackEncode Request where
  toMsgpack (Request m p) =
    ObjectArray [toMsgpack m, toMsgpack p]

data TrackedRequest =
  TrackedRequest {
    id :: RequestId,
    request :: Request
  }
  deriving stock (Eq, Show)

formatReq :: Request -> Text
formatReq (Request (RpcMethod method) payload) =
  [exon|#{method} #{show payload}|]

formatTrackedReq :: TrackedRequest -> Text
formatTrackedReq (TrackedRequest (RequestId i) req) =
  [exon|<#{show i}> #{formatReq req}|]
