-- |Codec data type for the result type of @nvim_get_mode@.
module Ribosome.Data.Mode where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |Codec data type for the result type of @nvim_get_mode@.
data NvimMode =
  NvimMode {
    mode :: Text,
    blocking :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)
