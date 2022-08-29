-- |Events sent from Neovim to the host.
module Ribosome.Host.Data.Event where

import Data.MessagePack (Object)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |The name of an event, which corresponds to the RPC method in the payload.
newtype EventName =
  EventName { unEventName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

-- |An event is an RPC notification sent by Neovim that is not intended to be dispatched to a named handler, but
-- consumed in a broadcasting fashion.
--
-- Since they aren't marked as such, the host treats any notification with an unknown method name as an event.
--
-- Events can be consumed with 'Conc.Consume' and 'Conc.subscribe'.
data Event =
  Event {
    name :: EventName,
    payload :: [Object]
  }
  deriving stock (Eq, Show)
