-- |Codec data type for @vim_get_windows@.
module Ribosome.Data.WindowConfig where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |Codec data type for @vim_get_windows@.
data WindowConfig =
  WindowConfig {
     relative :: Text,
     focusable :: Bool,
     external :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)
