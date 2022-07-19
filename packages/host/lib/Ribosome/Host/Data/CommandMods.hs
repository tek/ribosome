-- |Special command parameter that exposes the used modifiers.
module Ribosome.Host.Data.CommandMods where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)

-- |When this type is used as a parameter of a command handler function, the RPC trigger uses the special token
-- @<q-mods>@ in the call.
--
-- This type then contains the list of pre-command modifiers specified by the user, like @:belowright@.
newtype CommandMods =
  CommandMods { unCommandMods :: Text }
  deriving stock (Eq, Show)
  deriving newtype (MsgpackDecode)
