{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ribosome.Msgpack.Decode(
  MsgpackDecode(..),
) where

import GHC.Generics
-- import GHC.Generics (
--   Generic,
--   Rep,
--   (:*:)(..),
--   M1(..),
--   D1,
--   C1,
--   S1,
--   K1(..),
--   Selector,
--   Constructor,
--   selName,
--   to,
--   conIsRecord,
--   )
import Data.ByteString.Internal (unpackChars)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.MessagePack (Object(..))
import Data.Text.Prettyprint.Doc (Doc, (<+>), viaShow, pretty)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Ribosome.Msgpack.Util as Util (string)

type Err = Doc AnsiStyle

invalid :: String -> Object -> Either Err a
invalid msg obj =
  Left $ pretty (msg ++ ": ") <+> viaShow obj

missingRecordKey :: String -> Object -> Either Err a
missingRecordKey key =
  invalid $ "missing record key " ++ key ++ " in ObjectMap"

illegalType :: String -> Object -> Either Err a
illegalType tpe = invalid $ "illegal type for " ++ tpe

class MsgpackDecode a where
  fromMsgpack :: Object -> Either Err a
  default fromMsgpack :: (Generic a, GMsgpackDecode (Rep a)) => Object -> Either Err a
  fromMsgpack = fmap to . gMsgpackDecode

class GMsgpackDecode f where
  gMsgpackDecode :: Object -> Either Err (f a)

class MsgpackDecodeProd f where
  msgpackDecodeRecord :: Map Object Object -> Either Err (f a)
  msgpackDecodeProd :: [Object] -> Either Err ([Object], f a)

instance GMsgpackDecode f => GMsgpackDecode (D1 c f) where
  gMsgpackDecode = fmap M1 . gMsgpackDecode @f

instance (Constructor c, MsgpackDecodeProd f) => GMsgpackDecode (C1 c f) where
  gMsgpackDecode =
    fmap M1 . decode
    where
      isRec = conIsRecord (undefined :: t c f p)
      decode o@(ObjectMap om) =
        if isRec then msgpackDecodeRecord om else invalid "illegal ObjectMap for product" o
      decode o@(ObjectArray oa) =
        if isRec then invalid "illegal ObjectArray for record" o else decode'
        where
          decode' = do
            (rest, a) <- msgpackDecodeProd oa
            case rest of
              [] -> Right a
              _ -> invalid "too many values for product" o
      decode o =
        invalid "illegal Object for constructor" o

instance (MsgpackDecodeProd f, MsgpackDecodeProd g) => MsgpackDecodeProd (f :*: g) where
  msgpackDecodeRecord o = do
    left <- msgpackDecodeRecord o
    right <- msgpackDecodeRecord o
    return $ left :*: right
  msgpackDecodeProd o = do
    (rest, left) <- msgpackDecodeProd o
    (rest1, right) <- msgpackDecodeProd rest
    return (rest1, left :*: right)

instance (Selector s, GMsgpackDecode f) => MsgpackDecodeProd (S1 s f) where
  msgpackDecodeRecord o =
    maybe (missingRecordKey key (ObjectMap o)) (fmap M1 . gMsgpackDecode) (o !? Util.string key)
    where
      key = selName (undefined :: t s f p)
  msgpackDecodeProd (cur:rest) = do
    a <- gMsgpackDecode cur
    return $ (rest, M1 a)
  msgpackDecodeProd [] = invalid "too few values for product" ObjectNil

instance MsgpackDecode a => GMsgpackDecode (K1 i a) where
  gMsgpackDecode = fmap K1 . fromMsgpack

instance (Ord k, MsgpackDecode k, MsgpackDecode v) => MsgpackDecode (Map k v) where
  fromMsgpack (ObjectMap om) = do
    m <- traverse decodePair $ Map.toList om
    Right $ Map.fromList m
    where
      decodePair (k, v) = do
        k1 <- fromMsgpack k
        v1 <- fromMsgpack v
        return (k1, v1)
  fromMsgpack o = illegalType "Map" o

instance MsgpackDecode Int where
  fromMsgpack (ObjectInt i) = Right $ fromIntegral i
  fromMsgpack o = illegalType "Int" o

instance {-# OVERLAPPING #-} MsgpackDecode String where
  fromMsgpack (ObjectString os) = Right $ unpackChars os
  fromMsgpack o = illegalType "String" o

instance {-# OVERLAPPABLE #-} MsgpackDecode a => MsgpackDecode [a] where
  fromMsgpack (ObjectArray oa) = traverse fromMsgpack oa
  fromMsgpack o = illegalType "List" o
