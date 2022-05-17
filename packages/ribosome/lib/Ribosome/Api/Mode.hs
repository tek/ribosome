module Ribosome.Api.Mode where

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..), msgpackFromString)
import Ribosome.Host.Effect.Rpc (Rpc)

data NvimMode =
  Normal
  |
  Visual
  |
  Insert
  |
  Other Text
  deriving stock (Eq, Show)

instance IsString NvimMode where
  fromString "n" = Normal
  fromString "v" = Visual
  fromString "V" = Visual
  fromString "CTRL-V" = Visual
  fromString "i" = Insert
  fromString a = Other (toText a)

instance MsgpackDecode NvimMode where
  fromMsgpack =
    msgpackFromString "NvimMode"

mode ::
  Member Rpc r =>
  Sem r NvimMode
mode =
  vimCallFunction "mode" []

visualModeActive ::
  Member Rpc r =>
  Sem r Bool
visualModeActive =
  (== Visual) <$> mode
