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

-- |The payload of an RPC request.
data Request =
  Request {
    -- |The method, which is either the Neovim API function name or the internal identifier of a Ribosome handler.
    method :: RpcMethod,
    -- |The arguments.
    arguments :: [Object]
  }
  deriving stock (Eq, Show, Generic)

instance MsgpackEncode Request where
  toMsgpack (Request m p) =
    ObjectArray [toMsgpack m, toMsgpack p]

-- |An RPC request, which is a payload combined with a request ID.
data TrackedRequest =
  TrackedRequest {
    -- |The ID is used to associate the response with the sender.
    id :: RequestId,
    -- |The payload.
    request :: Request
  }
  deriving stock (Eq, Show)

formatReq :: Request -> Text
formatReq (Request (RpcMethod method) args) =
  [exon|#{method} #{show args}|]

formatTrackedReq :: TrackedRequest -> Text
formatTrackedReq (TrackedRequest (RequestId i) req) =
  [exon|<#{show i}> #{formatReq req}|]
