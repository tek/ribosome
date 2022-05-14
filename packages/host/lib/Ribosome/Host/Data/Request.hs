module Ribosome.Host.Data.Request where

import Data.MessagePack (Object (ObjectArray))
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

newtype RpcMethod =
  RpcMethod { unRpcMethod :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

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
