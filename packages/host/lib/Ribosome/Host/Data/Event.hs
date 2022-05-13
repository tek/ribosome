module Ribosome.Host.Data.Event where

import Data.MessagePack (Object)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype EventName =
  EventName { unEventName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

data Event =
  Event {
    name :: EventName,
    payload :: [Object]
  }
  deriving stock (Eq, Show)
