module Ribosome.Api.Mode where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode(..), msgpackFromString)
import Ribosome.Host.Api.Effect (vimCallFunction)

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
  fromMsgpack = msgpackFromString "NvimMode"

mode ::
  Member Rpc r =>
  m NvimMode
mode =
  vimCallFunction "mode" []

visualModeActive ::
  Member Rpc r =>
  m Bool
visualModeActive =
  (== Visual) <$> mode
