module Ribosome.Host.Data.Bar where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))

data Bar =
  Bar
  deriving stock (Eq, Show)

instance MsgpackDecode Bar where
  fromMsgpack _ =
    Right Bar
