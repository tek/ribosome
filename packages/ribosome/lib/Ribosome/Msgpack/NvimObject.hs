module Ribosome.Msgpack.NvimObject where

import Neovim (NvimObject(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

newtype NO a =
  NO { unNO :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (NFData)

instance (MsgpackEncode a, MsgpackDecode a, NFData a) => NvimObject (NO a) where
  toObject (NO a) = toMsgpack a
  fromObject a = NO <$> fromMsgpack a

(-$) :: (NO a -> b) -> a -> b
(-$) f a = f (NO a)
