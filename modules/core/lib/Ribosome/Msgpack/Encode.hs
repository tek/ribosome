{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Msgpack.Encode where

import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.MessagePack (Object(..))
import GHC.Generics (
  C1,
  Constructor,
  D1,
  K1(..),
  M1(..),
  Rep,
  S1,
  Selector,
  conIsRecord,
  from,
  selName,
  (:*:)(..),
  (:+:)(..),
  )
import Path (Path, toFilePath)
import qualified Ribosome.Msgpack.Util as Util (assembleMap, string, text)

class MsgpackEncode a where
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

instance (Ord k, MsgpackEncode k, MsgpackEncode v) => MsgpackEncode (Map k v) where
  toMsgpack = ObjectMap . Map.fromList . fmap (bimap toMsgpack toMsgpack) . Map.toList

instance MsgpackEncode Int where
  toMsgpack = ObjectInt . fromIntegral

instance MsgpackEncode Int64 where
  toMsgpack = ObjectInt . fromIntegral

instance MsgpackEncode Float where
  toMsgpack = ObjectFloat

instance MsgpackEncode Double where
  toMsgpack = ObjectDouble

instance {-# OVERLAPPING #-} MsgpackEncode String where
  toMsgpack = Util.string

instance {-# OVERLAPPABLE #-} MsgpackEncode a => MsgpackEncode [a] where
  toMsgpack = ObjectArray . fmap toMsgpack

instance MsgpackEncode a => MsgpackEncode (NonEmpty a) where
  toMsgpack = toMsgpack . NonEmpty.toList

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
  toMsgpack (a, b) = ObjectArray [toMsgpack a, toMsgpack b]

instance MsgpackEncode (Path b t) where
  toMsgpack = ObjectString . encodeUtf8 . toFilePath

instance IsString Object where
  fromString = ObjectString . encodeUtf8
