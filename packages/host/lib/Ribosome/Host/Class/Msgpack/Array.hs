module Ribosome.Host.Class.Msgpack.Array where

import Data.MessagePack (Object)
import Data.Sequence ((|>))

import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

newtype Acc =
  Acc { unAcc :: Seq Object }
  deriving stock (Eq, Show)

class MsgpackArray a where
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
