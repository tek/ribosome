{-# LANGUAGE TypeOperators #-}

module Ribosome.Msgpack.Decode where

import Control.Monad.DeepError (MonadDeepError, hoistEitherWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString (toString)
import Data.Either.Combinators (mapLeft)
import Data.Int (Int64)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.MessagePack (Object(..))
import Data.Text.Prettyprint.Doc (pretty)
import GHC.Generics (
  C1,
  Constructor,
  D1,
  Generic,
  K1(..),
  M1(..),
  Rep,
  S1,
  Selector,
  conIsRecord,
  selName,
  to,
  (:*:)(..),
  )

import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Msgpack.Error as DecodeError (DecodeError(Failed))
import Ribosome.Msgpack.Util (Err)
import qualified Ribosome.Msgpack.Util as Util (illegalType, invalid, missingRecordKey, string)

class MsgpackDecode a where
  fromMsgpack :: Object -> Either Err a
  default fromMsgpack :: (Generic a, GMsgpackDecode (Rep a)) => Object -> Either Err a
  fromMsgpack = fmap to . gMsgpackDecode

  missingKey :: String -> Object -> Either Err a
  missingKey = Util.missingRecordKey

class GMsgpackDecode f where
  gMsgpackDecode :: Object -> Either Err (f a)

  gMissingKey :: String -> Object -> Either Err (f a)
  gMissingKey = Util.missingRecordKey

class MsgpackDecodeProd f where
  msgpackDecodeRecord :: Map Object Object -> Either Err (f a)
  msgpackDecodeProd :: [Object] -> Either Err ([Object], f a)

instance (GMsgpackDecode f) => GMsgpackDecode (D1 c f) where
  gMsgpackDecode =
    fmap M1 . gMsgpackDecode @f

instance (Constructor c, MsgpackDecodeProd f) => GMsgpackDecode (C1 c f) where
  gMsgpackDecode =
    fmap M1 . decode
    where
      isRec = conIsRecord (undefined :: t c f p)
      decode o@(ObjectMap om) =
        if isRec then msgpackDecodeRecord om else Util.invalid "illegal ObjectMap for product" o
      decode o | isRec =
        Util.invalid "illegal non-ObjectMap for record" o
      decode o =
        msgpackDecodeProd (prod o) >>= check
        where
          check ([], a) = Right a
          check _ = Util.invalid "too many values for product" o
          prod (ObjectArray oa) = oa
          prod ob = [ob]

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
    M1 <$> maybe (gMissingKey key (ObjectMap o)) gMsgpackDecode (o !? Util.string key)
    where
      key = selName (undefined :: t s f p)
  msgpackDecodeProd (cur:rest) = do
    a <- gMsgpackDecode cur
    return (rest, M1 a)
  msgpackDecodeProd [] = Util.invalid "too few values for product" ObjectNil

instance MsgpackDecode a => GMsgpackDecode (K1 i a) where
  gMsgpackDecode = fmap K1 . fromMsgpack

  gMissingKey key =
    fmap K1 . missingKey key

instance (Ord k, MsgpackDecode k, MsgpackDecode v) => MsgpackDecode (Map k v) where
  fromMsgpack (ObjectMap om) = do
    m <- traverse decodePair $ Map.toList om
    Right $ Map.fromList m
    where
      decodePair (k, v) = do
        k1 <- fromMsgpack k
        v1 <- fromMsgpack v
        return (k1, v1)
  fromMsgpack o = Util.illegalType "Map" o

msgpackIntegral :: (Integral a, Read a) => Object -> Either Err a
msgpackIntegral (ObjectInt i) = Right $ fromIntegral i
msgpackIntegral (ObjectUInt i) = Right $ fromIntegral i
msgpackIntegral (ObjectString s) = mapLeft pretty . readEither . ByteString.toString $ s
msgpackIntegral o = Util.illegalType "Integral" o

instance MsgpackDecode Int where
  fromMsgpack = msgpackIntegral

instance MsgpackDecode Int64 where
  fromMsgpack = msgpackIntegral

instance MsgpackDecode Float where
  fromMsgpack (ObjectFloat a) = Right a
  fromMsgpack o = Util.illegalType "Float" o

instance {-# OVERLAPPING #-} MsgpackDecode String where
  fromMsgpack (ObjectString os) = Right $ ByteString.toString os
  fromMsgpack o = Util.illegalType "Text" o

instance {-# OVERLAPPABLE #-} MsgpackDecode a => MsgpackDecode [a] where
  fromMsgpack (ObjectArray oa) = traverse fromMsgpack oa
  fromMsgpack o = Util.illegalType "List" o

instance MsgpackDecode Text where
  fromMsgpack (ObjectString os) = Right (decodeUtf8 os)
  fromMsgpack o = Util.illegalType "Text" o

instance MsgpackDecode ByteString where
  fromMsgpack (ObjectString os) = Right os
  fromMsgpack o = Util.illegalType "ByteString" o

instance MsgpackDecode a => MsgpackDecode (Maybe a) where
  fromMsgpack ObjectNil = Right Nothing
  fromMsgpack o = Just <$> fromMsgpack o

  missingKey _ _ = Right Nothing

instance MsgpackDecode Bool where
  fromMsgpack (ObjectBool a) = Right a
  fromMsgpack o = Util.illegalType "Bool" o

instance MsgpackDecode () where
  fromMsgpack _ = Right ()

instance MsgpackDecode Object where
  fromMsgpack = Right

instance (MsgpackDecode a, MsgpackDecode b) => MsgpackDecode (a, b) where
  fromMsgpack (ObjectArray [a, b]) =
    (,) <$> fromMsgpack a <*> fromMsgpack b
  fromMsgpack o@(ObjectArray _) =
    Util.invalid "invalid array length for pair" o
  fromMsgpack o =
    Util.illegalType "pair" o

fromMsgpack' :: (MonadDeepError e DecodeError m, MsgpackDecode a) => Object -> m a
fromMsgpack' =
  hoistEitherWith DecodeError.Failed . fromMsgpack
