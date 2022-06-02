module Ribosome.Host.Data.Execution where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

data Execution =
  Sync
  |
  Async
  deriving stock (Eq, Show, Enum, Bounded)

instance MsgpackEncode Execution where
  toMsgpack exec =
    toMsgpack (exec == Sync)

instance MsgpackDecode Execution where
  fromMsgpack o =
    fromMsgpack o <&> \case
      True -> Sync
      False -> Async
