module Ribosome.Host.Data.Request where

import Data.MessagePack (Object)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype RpcMethod =
  RpcMethod { unRpcMethod :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

newtype RequestId =
  RequestId { unRequestId :: Int64 }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, MsgpackDecode, MsgpackEncode)

data Request a =
  Request {
    method :: RpcMethod,
    payload :: [Object]
  }
  deriving stock (Eq, Show)

data TrackedRequest a =
  TrackedRequest {
    id :: RequestId,
    request :: Request a
  }
  deriving stock (Eq, Show)
