module Ribosome.Host.Data.CommandRegister where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)

newtype CommandRegister =
  CommandRegister { unCommandRegister :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode)
