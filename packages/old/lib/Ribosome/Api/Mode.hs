module Ribosome.Api.Mode where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Decode (MsgpackDecode(..), msgpackFromString)
import Ribosome.Nvim.Api.IO (vimCallFunction)

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
  NvimE e m =>
  m NvimMode
mode =
  vimCallFunction "mode" []

visualModeActive ::
  NvimE e m =>
  m Bool
visualModeActive =
  (== Visual) <$> mode
