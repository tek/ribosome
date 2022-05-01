module Ribosome.Host.Data.LuaRef where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype LuaRef =
  LuaRef { unLuaRef :: Int64 }
  deriving stock (Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
