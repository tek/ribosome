-- |Special command parameter that enables command chaining.
module Ribosome.Host.Data.Bar where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))

-- |When this type is used as a parameter of a command handler function, the command is declared with the @-bar@ option,
-- allowing other commands to be chained after it with @|@.
--
-- This has no effect on the execution.
data Bar =
  Bar
  deriving stock (Eq, Show)

instance MsgpackDecode Bar where
  fromMsgpack _ =
    Right Bar
