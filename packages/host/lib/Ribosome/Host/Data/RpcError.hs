module Ribosome.Host.Data.RpcError where

import Log (Severity (Error))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data RpcError =
  Unexpected Text
  |
  Decode Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

instance IsString RpcError where
  fromString =
    Unexpected . toText

instance ToErrorMessage RpcError where
  toErrorMessage = \case
    Unexpected e ->
      ErrorMessage "Nvim API failure" [e] Error
    Decode e ->
      ErrorMessage "Msgpack decoding failed" [e] Error

rpcErrorMessage :: RpcError -> Text
rpcErrorMessage = \case
  Unexpected e -> e
  Decode e -> e
