module Ribosome.Data.WindowConfig where

import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)

data WindowConfig =
  WindowConfig {
     relative :: Text,
     focusable :: Bool,
     external :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)
