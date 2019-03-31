module Ribosome.Msgpack.NvimObject(
  NO(..),
  (-$),
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Neovim (NvimObject(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

newtype NO a =
  NO { unNO :: a }
  deriving (Eq, Show, Generic, NFData)

instance (MsgpackEncode a, MsgpackDecode a, NFData a) => NvimObject (NO a) where
  toObject (NO a) = toMsgpack a
  fromObject a = NO <$> fromMsgpack a

(-$) :: (NO a -> b) -> a -> b
(-$) f a = f (NO a)
