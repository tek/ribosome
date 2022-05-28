module Ribosome.Host.Data.RpcError where

import Log (Severity (Error))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

newtype RpcError =
  RpcError { unRpcError :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, MsgpackEncode, MsgpackDecode)

instance ToErrorMessage RpcError where
  toErrorMessage (RpcError e) =
    ErrorMessage "Nvim API failure" [e] Error
