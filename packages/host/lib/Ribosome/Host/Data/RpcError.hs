module Ribosome.Host.Data.RpcError where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype RpcError =
  RpcError { unRpcError :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, MsgpackEncode, MsgpackDecode)
