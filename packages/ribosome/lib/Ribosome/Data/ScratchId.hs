-- |The ID type used to store active scratch buffers.
module Ribosome.Data.ScratchId where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

-- |The ID type used to store active scratch buffers.
newtype ScratchId =
  ScratchId { unScratchId :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)
