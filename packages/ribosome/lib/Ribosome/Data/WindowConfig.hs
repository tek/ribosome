module Ribosome.Data.WindowConfig where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

data WindowConfig =
  WindowConfig {
     relative :: Text,
     focusable :: Bool,
     external :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)
