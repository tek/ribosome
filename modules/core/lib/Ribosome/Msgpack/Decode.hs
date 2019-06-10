module Ribosome.Msgpack.Decode where

import Control.Monad.DeepError (MonadDeepError, hoistEitherWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString (toString)
import Data.Either.Combinators (mapLeft)
import Data.Int (Int64)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map (empty, fromList, toList)
import Data.MessagePack (Object(..))
import Data.Text.Prettyprint.Doc (pretty, viaShow)
import GHC.Float (double2Float)
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
  (:+:)(..),
  )
import Neovim (CommandArguments)
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile)

import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Msgpack.Error as DecodeError (DecodeError(Failed))
import Ribosome.Msgpack.Util (Err)
import qualified Ribosome.Msgpack.Util as Util (binary, illegalType, invalid, lookupObjectMap, missingRecordKey, string)

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

instance (GMsgpackDecode f, GMsgpackDecode g) => GMsgpackDecode (f :+: g) where
  gMsgpackDecode o = fromRight (L1 <$> gMsgpackDecode @f o) (Right . R1 <$> gMsgpackDecode @g o)

-- TODO use Proxy instead of undefined
instance (Selector s, GMsgpackDecode f) => MsgpackDecodeProd (S1 s f) where
  msgpackDecodeRecord o =
    M1 <$> maybe (gMissingKey key (ObjectMap o)) gMsgpackDecode (Util.lookupObjectMap key o)
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
  missingKey _ _ = Right Map.empty

integralFromString ::
  Integral a =>
  Read a =>
  ByteString ->
  Either Err a
integralFromString =
  mapLeft pretty . readEither . ByteString.toString

msgpackIntegral ::
  Integral a =>
  Read a =>
  Object ->
  Either Err a
msgpackIntegral (ObjectInt i) = Right $ fromIntegral i
msgpackIntegral (ObjectUInt i) = Right $ fromIntegral i
msgpackIntegral (ObjectString s) = integralFromString s
msgpackIntegral (ObjectBinary s) = integralFromString s
msgpackIntegral o = Util.illegalType "Integral" o

msgpackText :: ConvertUtf8 t ByteString => Text -> (t -> Either Err a) -> Object -> Either Err a
msgpackText typeName decode =
  run
  where
    run (ObjectString os) = decode $ decodeUtf8 os
    run (ObjectBinary os) = decode $ decodeUtf8 os
    run o = Util.illegalType typeName o

instance MsgpackDecode Int where
  fromMsgpack = msgpackIntegral

instance MsgpackDecode Int64 where
  fromMsgpack = msgpackIntegral

instance MsgpackDecode Float where
  fromMsgpack (ObjectFloat a) = Right a
  fromMsgpack (ObjectDouble a) = Right (double2Float a)
  fromMsgpack (ObjectInt a) = Right (fromIntegral a)
  fromMsgpack (ObjectUInt a) = Right (fromIntegral a)
  fromMsgpack o = Util.illegalType "Float" o

instance {-# OVERLAPPING #-} MsgpackDecode String where
  fromMsgpack = msgpackText "String" Right

instance {-# OVERLAPPABLE #-} MsgpackDecode a => MsgpackDecode [a] where
  fromMsgpack (ObjectArray oa) = traverse fromMsgpack oa
  fromMsgpack o = Util.illegalType "List" o
  missingKey _ _ = Right []

instance MsgpackDecode Text where
  fromMsgpack =
    msgpackText "Text" Right

instance MsgpackDecode ByteString where
  fromMsgpack (ObjectString os) = Right os
  fromMsgpack (ObjectBinary os) = Right os
  fromMsgpack o = Util.illegalType "ByteString" o

instance MsgpackDecode Char where
  fromMsgpack o =
    msgpackText "Char" check o
    where
      check [c] = Right c
      check _ = Util.invalid "multiple characters when decoding Char" o

instance MsgpackDecode a => MsgpackDecode (Maybe a) where
  fromMsgpack ObjectNil = Right Nothing
  fromMsgpack o = Just <$> fromMsgpack o
  missingKey _ _ = Right Nothing

instance (MsgpackDecode a, MsgpackDecode b) => MsgpackDecode (Either a b) where
  fromMsgpack o =
    fromRight (Left <$> fromMsgpack o) (Right . Right <$> fromMsgpack o)

instance MsgpackDecode Bool where
  fromMsgpack (ObjectBool a) = Right a
  fromMsgpack (ObjectInt 0) = Right False
  fromMsgpack (ObjectInt 1) = Right True
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

instance MsgpackDecode CommandArguments where

class DecodePath b t where
  decodePath :: FilePath -> Either SomeException (Path b t)

instance DecodePath Abs File where
  decodePath =
    parseAbsFile

instance DecodePath Abs Dir where
  decodePath =
    parseAbsDir

instance DecodePath Rel File where
  decodePath =
    parseRelFile

instance DecodePath Rel Dir where
  decodePath =
    parseRelDir

decodePathE ::
  ∀ b t .
  DecodePath b t =>
  Text ->
  Either Err (Path b t)
decodePathE =
  first viaShow . decodePath . toString

instance DecodePath b t => MsgpackDecode (Path b t) where
  fromMsgpack =
    msgpackText "Path" decodePathE

fromMsgpack' ::
  ∀ a e m.
  MonadDeepError e DecodeError m =>
  MsgpackDecode a =>
  Object ->
  m a
fromMsgpack' =
  hoistEitherWith DecodeError.Failed . fromMsgpack

msgpackFromString :: IsString a => Text -> Object -> Either Err a
msgpackFromString name o =
  adapt $ fromMsgpack o
  where
    adapt (Right a) =
      Right $ fromString a
    adapt (Left _) =
      Util.illegalType name o
