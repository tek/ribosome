module Ribosome.Host.Data.RpcError where

import Exon (exon)
import Log (Severity (Error))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))

data RpcError =
  Unexpected Text
  |
  Api RpcMethod Text
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
      Report "Internal error" [e] Error
    Api (RpcMethod m) e ->
      Report "Nvim API failure" [m, e] Error
    Decode e ->
      Report "Msgpack decoding failed" [e] Error

rpcError :: RpcError -> Text
rpcError = \case
  Unexpected e -> e
  Api (RpcMethod m) e -> [exon|#{m}: #{e}|]
  Decode e -> e
