module Ribosome.Api.Mode where

import Ribosome.Data.Mode (NvimMode)
import Ribosome.Host.Api.Effect (nvimGetMode, vimCallFunction)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..), msgpackFromString)
import Ribosome.Host.Effect.Rpc (Rpc)

data SimpleMode =
  Normal
  |
  Visual
  |
  Insert
  |
  Other Text
  deriving stock (Eq, Show)

instance IsString SimpleMode where
  fromString "n" = Normal
  fromString "v" = Visual
  fromString "V" = Visual
  fromString "CTRL-V" = Visual
  fromString "i" = Insert
  fromString a = Other (toText a)

instance MsgpackDecode SimpleMode where
  fromMsgpack =
    msgpackFromString "SimpleMode"

simpleMode ::
  Member Rpc r =>
  Sem r SimpleMode
simpleMode =
  vimCallFunction "mode" []

visualModeActive ::
  Member Rpc r =>
  Sem r Bool
visualModeActive =
  (== Visual) <$> simpleMode

mode ::
  Member Rpc r =>
  Sem r NvimMode
mode =
  nvimGetMode
