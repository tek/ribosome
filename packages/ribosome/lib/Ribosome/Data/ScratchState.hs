module Ribosome.Data.ScratchState where

import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype ScratchId =
  ScratchId { unScratchId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

data ScratchState =
  ScratchState {
    id :: ScratchId,
    buffer :: Buffer,
    window :: Window,
    previous :: Window,
    tab :: Maybe Tabpage
  }
  deriving stock (Eq, Show)
