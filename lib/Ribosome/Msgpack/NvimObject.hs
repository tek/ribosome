module Ribosome.Msgpack.NvimObject(
  NO(..),
  (-$),
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Neovim (NvimObject(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(..))

newtype NO a =
  NO a
  deriving (Eq, Show, Generic, NFData)

instance (MsgpackEncode a, MsgpackDecode a, NFData a) => NvimObject (NO a) where
  toObject (NO a) = toMsgpack a
  fromObject a = NO <$> fromMsgpack a

(-$) :: (NO a -> b) -> a -> b
(-$) f a = f (NO a)
