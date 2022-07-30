{-# options_haddock prune #-}

-- |Encoding values to MessagePack format
module Ribosome.Host.Class.Msgpack.Encode where

import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.MessagePack (Object (..))
import GHC.Generics (
  C1,
  Constructor,
  D1,
  K1 (..),
  M1 (..),
  Rep,
  S1,
  Selector,
  conIsRecord,
  from,
  selName,
  (:*:) (..),
  (:+:) (..),
  )
import Path (Path, toFilePath)
import Time (MicroSeconds, MilliSeconds (unMilliSeconds), NanoSeconds (unNanoSeconds), Seconds, unMicroSeconds, unSeconds)

import qualified Ribosome.Host.Class.Msgpack.Util as Util (assembleMap, string, text)

-- |Class of values that can be encoded to MessagePack 'Object's.
class MsgpackEncode a where
  -- |Convert a value to MessagePack.
  --
  -- The default implementation uses generic derivation.
  toMsgpack :: a -> Object
  default toMsgpack :: (Generic a, GMsgpackEncode (Rep a)) => a -> Object
  toMsgpack = gMsgpackEncode . from

class GMsgpackEncode f where
  gMsgpackEncode :: f a -> Object

class MsgpackEncodeProd f where
  msgpackEncodeRecord :: f a -> [(String, Object)]
  msgpackEncodeProd :: f a -> [Object]

instance GMsgpackEncode f => GMsgpackEncode (D1 c f) where
  gMsgpackEncode = gMsgpackEncode . unM1

prodOrNewtype :: MsgpackEncodeProd f => f a -> Object
prodOrNewtype =
  wrap . msgpackEncodeProd
  where
    wrap [a] = a
    wrap as = ObjectArray as

instance (Constructor c, MsgpackEncodeProd f) => GMsgpackEncode (C1 c f) where
  gMsgpackEncode c =
    f $ unM1 c
    where
      f = if conIsRecord c then Util.assembleMap . msgpackEncodeRecord else prodOrNewtype

instance (MsgpackEncodeProd f, MsgpackEncodeProd g) => MsgpackEncodeProd (f :*: g) where
  msgpackEncodeRecord (f :*: g) = msgpackEncodeRecord f <> msgpackEncodeRecord g
  msgpackEncodeProd (f :*: g) = msgpackEncodeProd f <> msgpackEncodeProd g

instance (GMsgpackEncode f, GMsgpackEncode g) => GMsgpackEncode (f :+: g) where
  gMsgpackEncode (L1 a) = gMsgpackEncode a
  gMsgpackEncode (R1 a) = gMsgpackEncode a

instance (Selector s, GMsgpackEncode f) => MsgpackEncodeProd (S1 s f) where
  msgpackEncodeRecord s@(M1 f) =
    [(dropWhile ('_' ==) (selName s), gMsgpackEncode f), (selName s, gMsgpackEncode f)]
  msgpackEncodeProd (M1 f) = [gMsgpackEncode f]

instance MsgpackEncode a => GMsgpackEncode (K1 i a) where
  gMsgpackEncode = toMsgpack . unK1

instance (
    MsgpackEncode k,
    MsgpackEncode v
  ) => MsgpackEncode (Map k v) where
  toMsgpack = ObjectMap . Map.fromList . fmap (bimap toMsgpack toMsgpack) . Map.toList

instance MsgpackEncode Integer where
  toMsgpack = ObjectInt . fromInteger

instance MsgpackEncode Int where
  toMsgpack = ObjectInt . fromIntegral

instance MsgpackEncode Int64 where
  toMsgpack = ObjectInt

instance MsgpackEncode Float where
  toMsgpack = ObjectFloat

instance MsgpackEncode Double where
  toMsgpack = ObjectDouble

instance {-# overlapping #-} MsgpackEncode String where
  toMsgpack = Util.string

instance {-# overlappable #-} MsgpackEncode a => MsgpackEncode [a] where
  toMsgpack = ObjectArray . fmap toMsgpack

instance MsgpackEncode a => MsgpackEncode (NonEmpty a) where
  toMsgpack = toMsgpack . NonEmpty.toList

instance MsgpackEncode a => MsgpackEncode (Seq a) where
  toMsgpack = toMsgpack . toList

instance MsgpackEncode Text where
  toMsgpack = Util.text

instance MsgpackEncode a => MsgpackEncode (Maybe a) where
  toMsgpack = maybe ObjectNil toMsgpack

instance MsgpackEncode Bool where
  toMsgpack = ObjectBool

instance MsgpackEncode () where
  toMsgpack _ = ObjectNil

instance MsgpackEncode Object where
  toMsgpack = id

instance MsgpackEncode ByteString where
  toMsgpack = ObjectString

instance (MsgpackEncode a, MsgpackEncode b) => MsgpackEncode (a, b) where
  toMsgpack (a, b) =
    ObjectArray [toMsgpack a, toMsgpack b]

instance (MsgpackEncode a, MsgpackEncode b, MsgpackEncode c) => MsgpackEncode (a, b, c) where
  toMsgpack (a, b, c) =
    ObjectArray [toMsgpack a, toMsgpack b, toMsgpack c]

instance MsgpackEncode (Path b t) where
  toMsgpack = ObjectString . encodeUtf8 . toFilePath

instance MsgpackEncode NanoSeconds where
  toMsgpack =
    toMsgpack . unNanoSeconds

instance MsgpackEncode MicroSeconds where
  toMsgpack =
    toMsgpack . unMicroSeconds

instance MsgpackEncode MilliSeconds where
  toMsgpack =
    toMsgpack . unMilliSeconds

instance MsgpackEncode Seconds where
  toMsgpack =
    toMsgpack . unSeconds
