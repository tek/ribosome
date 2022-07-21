-- |RPC message execution
module Ribosome.Host.Data.Execution where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

-- |This type indicates the execution style that Neovim should be instructed to use for RPC messages – synchronous
-- requests that block Neovim until a result is returned and asynchronous notifications.
--
-- 
data Execution =
  -- |RPC Request
  Sync
  |
  -- |RPC Notification
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
