module Ribosome.Host.Data.CommandMods where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)

newtype CommandMods =
  CommandMods { unCommandMods :: Text }
  deriving stock (Eq, Show)
  deriving newtype (MsgpackDecode)
