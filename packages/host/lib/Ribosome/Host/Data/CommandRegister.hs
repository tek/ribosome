-- |Special command parameter that exposes the used register.
module Ribosome.Host.Data.CommandRegister where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)

-- |When this type is used as a parameter of a command handler function, the RPC trigger uses the special token
-- @<reg>@ in the call.
--
-- This type then contains the name of the register specified by the user.
newtype CommandRegister =
  CommandRegister { unCommandRegister :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode)
