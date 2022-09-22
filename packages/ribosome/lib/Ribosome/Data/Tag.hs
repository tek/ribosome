module Ribosome.Data.Tag where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |The return type of 'taglist', representing a ctags-like entry of a tags file.
data Tag =
  Tag {
    name :: Text,
    filename :: Text,
    kind :: Text,
    static :: Bool,
    cmd :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)
