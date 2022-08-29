-- |The basic error type for the plugin host.
module Ribosome.Host.Data.RpcError where

import Data.MessagePack (Object)
import qualified Data.Text as Text
import Exon (exon)
import Log (Severity (Error))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))

-- |The basic error type for the plugin host, used by the listener, 'Rpc' and several other components.
data RpcError =
  -- |An error that is supposed to be prevented by the implementation.
  Unexpected Text
  |
  -- |The Neovim API encountered a problem.
  Api RpcMethod [Object] Text
  |
  -- |A request was instructed to use the wrong decoder or the remote data was invalid.
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
    Api (RpcMethod m) args e ->
      Report "Nvim API failure" [m, show args, e] Error
    Decode e ->
      Report "Msgpack decoding failed" [e] Error

-- |Extract an error message from an 'RpcError'.
rpcError :: RpcError -> Text
rpcError = \case
  Unexpected e -> e
  Api (RpcMethod m) args e -> [exon|#{m}: #{e}(#{Text.intercalate ", " (show <$> args)})|]
  Decode e -> e
