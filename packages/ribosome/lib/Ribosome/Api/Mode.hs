-- |API functions for obtaining Neovim's current mode.
module Ribosome.Api.Mode where

import Ribosome.Data.Mode (NvimMode)
import Ribosome.Host.Api.Data (nvimGetMode, vimCallFunction)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..))
import Ribosome.Host.Class.Msgpack.Util (decodeString)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |An encoding of Neovim's mode for only the most basic variants.
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
    decodeString

-- |Get the current mode as a 'SimpleMode'.
simpleMode ::
  Member Rpc r =>
  Sem r SimpleMode
simpleMode =
  vimCallFunction "mode" []

-- |Indicate whether Neovim is in visual mode.
visualModeActive ::
  Member Rpc r =>
  Sem r Bool
visualModeActive =
  (== Visual) <$> simpleMode

-- |Get the current mode.
mode ::
  Member Rpc r =>
  Sem r NvimMode
mode =
  nvimGetMode
