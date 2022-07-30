{-# options_haddock prune #-}

-- |Decoding values from MessagePack format
module Ribosome.Host.Class.Msgpack.Decode where

import qualified Data.Map.Strict as Map (empty, fromList, toList)
import Data.MessagePack (Object (..))
import Exon (exon)
import GHC.Float (double2Float, float2Double)
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
  selName,
  to,
  (:*:) (..),
  (:+:) (..),
  )
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile)
import Prelude hiding (to)
import Time (MicroSeconds, MilliSeconds, NanoSeconds, Seconds (Seconds))

import qualified Ribosome.Host.Class.Msgpack.Util as Util (illegalType, invalid, lookupObjectMap, missingRecordKey)

-- |Class of values that can be decoded from MessagePack 'Object's.
class MsgpackDecode a where
  -- |Attempt to decode an 'Object', returning an error message in a 'Left' if the data is incompatible.
  --
  -- The default implementation uses generic derivation.
  fromMsgpack :: Object -> Either Text a
  default fromMsgpack :: (Generic a, GMsgpackDecode (Rep a)) => Object -> Either Text a
  fromMsgpack = fmap to . gMsgpackDecode

  -- |Utility method called by the generic machinery when a record key is missing.
  missingKey :: String -> Object -> Either Text a
  missingKey = Util.missingRecordKey

-- |Pattern synonym for decoding an 'Object'.
pattern Msgpack :: ∀ a . MsgpackDecode a => a -> Object
pattern Msgpack a <- (fromMsgpack -> Right a)

class GMsgpackDecode f where
  gMsgpackDecode :: Object -> Either Text (f a)

  gMissingKey :: String -> Object -> Either Text (f a)
  gMissingKey = Util.missingRecordKey

class MsgpackDecodeProd f where
  msgpackDecodeRecord :: Map Object Object -> Either Text (f a)
  msgpackDecodeProd :: [Object] -> Either Text ([Object], f a)

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
    pure $ left :*: right
  msgpackDecodeProd o = do
    (rest, left) <- msgpackDecodeProd o
    (rest1, right) <- msgpackDecodeProd rest
    pure (rest1, left :*: right)

instance (GMsgpackDecode f, GMsgpackDecode g) => GMsgpackDecode (f :+: g) where
  gMsgpackDecode o = fromRight (L1 <$> gMsgpackDecode @f o) (Right . R1 <$> gMsgpackDecode @g o)

-- TODO use Proxy instead of undefined
instance (Selector s, GMsgpackDecode f) => MsgpackDecodeProd (S1 s f) where
  msgpackDecodeRecord o =
    M1 <$> maybe (gMissingKey key (ObjectMap o)) gMsgpackDecode lookup
    where
      lookup =
        Util.lookupObjectMap key o <|> lookupUnderscore
      lookupUnderscore =
        if hasUnderscore
        then Util.lookupObjectMap (dropWhile ('_' ==) key) o
        else Nothing
      hasUnderscore =
        take 1 key == "_"
      key =
        selName (undefined :: t s f p)
  msgpackDecodeProd (cur:rest) = do
    a <- gMsgpackDecode cur
    pure (rest, M1 a)
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
        pure (k1, v1)
  fromMsgpack o = Util.illegalType "Map" o
  missingKey _ _ = Right Map.empty

integralFromString ::
  Read a =>
  ByteString ->
  Either Text a
integralFromString =
  readEither . decodeUtf8

msgpackIntegral ::
  Integral a =>
  Read a =>
  Object ->
  Either Text a
msgpackIntegral (ObjectInt i) = Right $ fromIntegral i
msgpackIntegral (ObjectUInt i) = Right $ fromIntegral i
msgpackIntegral (ObjectString s) = integralFromString s
msgpackIntegral (ObjectBinary s) = integralFromString s
msgpackIntegral o = Util.illegalType "Integral" o

msgpackText :: ConvertUtf8 t ByteString => Text -> (t -> Either Text a) -> Object -> Either Text a
msgpackText typeName decode =
  run
  where
    run (ObjectString os) = decode $ decodeUtf8 os
    run (ObjectBinary os) = decode $ decodeUtf8 os
    run o = Util.illegalType typeName o

instance MsgpackDecode Integer where
  fromMsgpack = msgpackIntegral

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

instance MsgpackDecode Double where
  fromMsgpack (ObjectFloat a) = Right (float2Double a)
  fromMsgpack (ObjectDouble a) = Right a
  fromMsgpack (ObjectInt a) = Right (fromIntegral a)
  fromMsgpack (ObjectUInt a) = Right (fromIntegral a)
  fromMsgpack o = Util.illegalType "Double" o

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
      check :: [Char] -> Either Text Char
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

decodeTuple :: Int -> ([Object] -> Either (Maybe Text) a) -> Object -> Either Text a
decodeTuple i f = \case
  o@(ObjectArray oa) ->
    case f oa of
      Right a -> pure a
      Left Nothing -> Util.invalid [exon|invalid array length for #{show i}-tuple|] o
      Left (Just err) -> Left err
  o ->
    Util.illegalType [exon|#{show i}-tuple|] o

instance (MsgpackDecode a, MsgpackDecode b) => MsgpackDecode (a, b) where
  fromMsgpack =
    decodeTuple 2 \case
      [a, b] ->
        first Just ((,) <$> fromMsgpack a <*> fromMsgpack b)
      _ ->
        Left Nothing

instance (MsgpackDecode a, MsgpackDecode b, MsgpackDecode c) => MsgpackDecode (a, b, c) where
  fromMsgpack =
    decodeTuple 3 \case
      [a, b, c] ->
        first Just ((,,) <$> fromMsgpack a <*> fromMsgpack b <*> fromMsgpack c)
      _ ->
        Left Nothing

instance (MsgpackDecode a, MsgpackDecode b, MsgpackDecode c, MsgpackDecode d) => MsgpackDecode (a, b, c, d) where
  fromMsgpack =
    decodeTuple 4 \case
      [a, b, c, d] ->
        first Just ((,,,) <$> fromMsgpack a <*> fromMsgpack b <*> fromMsgpack c <*> fromMsgpack d)
      _ ->
        Left Nothing

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
  Either Text (Path b t)
decodePathE =
  first show . decodePath . toString

instance DecodePath b t => MsgpackDecode (Path b t) where
  fromMsgpack =
    msgpackText "Path" decodePathE

timeUnit ::
  Integral a =>
  Fractional a =>
  Text ->
  Object ->
  Either Text a
timeUnit name = \case
  Msgpack d -> Right (realToFrac @Double d)
  Msgpack i -> Right (fromIntegral @Int64 i)
  o -> Util.illegalType name o

instance MsgpackDecode NanoSeconds where
  fromMsgpack =
    timeUnit "NanoSeconds"

instance MsgpackDecode MicroSeconds where
  fromMsgpack =
    timeUnit "MicroSeconds"

instance MsgpackDecode MilliSeconds where
  fromMsgpack =
    timeUnit "MilliSeconds"

instance MsgpackDecode Seconds where
  fromMsgpack =
    fmap Seconds . fromMsgpack

msgpackFromString :: IsString a => Text -> Object -> Either Text a
msgpackFromString name o =
  case fromMsgpack o of
    Right a ->
      Right (fromString a)
    Left _ ->
      Util.illegalType name o
