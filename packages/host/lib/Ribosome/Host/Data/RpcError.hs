module Ribosome.Host.Data.RpcError where

import Log (Severity (Error))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))

data RpcError =
  Unexpected Text
  |
  Decode Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

instance IsString RpcError where
  fromString =
    Unexpected . toText

instance Reportable RpcError where
  toReport = \case
    Unexpected e ->
      Report "Nvim API failure" [e] Error
    Decode e ->
      Report "Msgpack decoding failed" [e] Error

rpcReport :: RpcError -> Text
rpcReport = \case
  Unexpected e -> e
  Decode e -> e
