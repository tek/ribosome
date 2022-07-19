-- |Helper for encoding values to a heterogeneous MessagePack array.
module Ribosome.Host.Class.Msgpack.Array where

import Data.MessagePack (Object)
import Data.Sequence ((|>))

import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

newtype Acc =
  Acc { unAcc :: Seq Object }
  deriving stock (Eq, Show)

-- |This class provides a variadic method for encoding MessagePack arrays.
class MsgpackArray a where
  -- |Encode an arbitrary number of heterogeneously typed values to a single MessagePack array.
  -- This function is variadic, meaning that it takes an arbitrary number of arguments:
  --
  -- >>> msgpackArray (5 :: Int) ("error" :: Text) (3.14 :: Double) :: Object
  -- ObjectArray [ObjectInt 5, ObjectString "error", ObjectFloat 3.14]
  --
  -- This avoids the need to call 'Ribosome.toMsgpack' once for each element and then once more for the array.
  msgpackArray :: a

instance MsgpackArray (Acc -> [Object]) where
  msgpackArray =
    toList . unAcc

instance MsgpackArray (Acc -> Object) where
  msgpackArray =
    toMsgpack . unAcc

instance MsgpackArray (a -> a) where
  msgpackArray =
    id

instance (
    MsgpackEncode a,
    MsgpackArray (Acc -> b)
  ) => MsgpackArray (Acc -> a -> b) where
  msgpackArray (Acc m) a =
    msgpackArray @(Acc -> b) (Acc (m |> toMsgpack a))

instance {-# overlappable #-} (
    MsgpackEncode a,
    MsgpackArray (Acc -> b)
  ) => MsgpackArray (a -> b) where
  msgpackArray a =
    msgpackArray @(Acc -> b) (Acc [toMsgpack a])
