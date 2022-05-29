module Ribosome.Data.ScratchId where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype ScratchId =
  ScratchId { unScratchId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)
