{-# options_haddock prune #-}

-- |The class for decoding messagepack.
module Ribosome.Host.Class.Msgpack.Decode where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object (..))
import Exon (exon)
import Generics.SOP (I (I), NP (Nil, (:*)), NS (S, Z), SOP (SOP))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, gto)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  DatatypeInfo (ADT, Newtype),
  FieldInfo (FieldInfo),
  )
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, SomeBase, parseSomeDir, parseSomeFile)
import Time (MicroSeconds, MilliSeconds, NanoSeconds, Seconds (Seconds))

import Ribosome.Host.Class.Msgpack.Error (
  DecodeError,
  FieldError (FieldError, NestedFieldError),
  decodeIncompatible,
  incompatible,
  incompatibleCon,
  incompatibleShape,
  renderError,
  symbolText,
  toDecodeError,
  utf8Error,
  )
import Ribosome.Host.Class.Msgpack.Util (
  ReifySOP,
  ValidUtf8 (ValidUtf8),
  ValidUtf8String (ValidUtf8String),
  decodeByteString,
  decodeFractional,
  decodeIntegral,
  decodeUtf8Lenient,
  )

class GMsgpackDecode (dt :: DatatypeInfo) (ass :: [[Type]]) where
  gfromMsgpack :: Object -> Either FieldError (SOP I ass)

fromMsgpackGeneric ::
  ∀ a ass .
  Typeable a =>
  ReifySOP a ass =>
  GMsgpackDecode (GDatatypeInfoOf a) (GCode a) =>
  Object ->
  Either DecodeError a
fromMsgpackGeneric =
  toDecodeError . fmap gto . gfromMsgpack @(GDatatypeInfoOf a)

-- |Class of values that can be decoded from MessagePack 'Object's.
class MsgpackDecode a where
  -- |Decode a value from a MessagePack 'Object'.
    --
  -- The default implementation uses generic derivation.
  fromMsgpack :: Object -> Either DecodeError a
  default fromMsgpack ::
    Typeable a =>
    ReifySOP a ass =>
    GMsgpackDecode (GDatatypeInfoOf a) (GCode a) =>
    Object ->
    Either DecodeError a
  fromMsgpack = fromMsgpackGeneric

nestedDecode ::
  MsgpackDecode a =>
  Object ->
  Either FieldError a
nestedDecode o =
  first NestedFieldError (fromMsgpack o)

class DecodeProd (as :: [Type]) where
  decodeProd :: [Object] -> Either FieldError (NP I as)

instance DecodeProd '[] where
  decodeProd = \case
    [] ->
      Right Nil
    o ->
      incompatibleShape "product type" [exon|#{show (length o)} extra elements|]

instance (
    MsgpackDecode a,
    DecodeProd as
  ) => DecodeProd (a : as) where
    decodeProd = \case
      o : os -> do
        a <- nestedDecode o
        as <- decodeProd os
        pure (I a :* as)
      [] ->
        incompatibleShape "product type" "too few elements"

-- |This class decides what to return when a key in an 'ObjectMap' is missing for a corresponding record field.
--
-- Primarily used for 'Maybe' fields, since they should decode to 'Nothing' when the key is absent.
class MissingKey a where
  -- |Return a fallback value for a missing key in an 'ObjectMap'.
  missingKey :: String -> Map String Object -> Either FieldError a

instance {-# overlappable #-} MissingKey a where
  missingKey name _ =
    Left (FieldError [exon|Missing record field '#{toText name}'|])

instance MissingKey (Maybe a) where
  missingKey _ _ =
    Right Nothing

class DecodeRecord (fields :: [FieldInfo]) (as :: [Type]) where
  decodeRecord :: Map String Object -> Either FieldError (NP I as)

instance DecodeRecord '[] '[] where
  decodeRecord _ =
    Right Nil

instance (
    KnownSymbol name,
    MsgpackDecode a,
    MissingKey a,
    DecodeRecord fields as
  ) => DecodeRecord ('FieldInfo name : fields) (a : as) where
  decodeRecord os = do
    a <- lookupField
    as <- decodeRecord @fields os
    pure (I a :* as)
    where
      lookupField =
        case Map.lookup name os of
          Just o ->
            nestedDecode o
          Nothing ->
            missingKey name os
      name =
        symbolVal (Proxy @name)

class DecodeCtor (ctor :: ConstructorInfo) (as :: [Type]) where
  decodeCtor :: Object -> Either FieldError (NP I as)

instance (
    KnownSymbol name,
    DecodeProd as
  ) => DecodeCtor ('Constructor name) as where
    decodeCtor = \case
      ObjectArray os ->
        decodeProd @as os
      o ->
        incompatibleCon [exon|product constructor #{symbolText @name}|] o

instance (
    KnownSymbol name,
    MsgpackDecode l,
    MsgpackDecode r
  ) => DecodeCtor ('Infix name assoc fixity) [l, r] where
    decodeCtor = \case
      ObjectArray [obl, obr] -> do
        l <- nestedDecode obl
        r <- nestedDecode obr
        pure (I l :* I r :* Nil)
      ObjectArray os ->
        incompatibleShape desc [exon|Array with #{show (length os)} elements|]
      o ->
        incompatibleCon desc o
      where
        desc =
          [exon|infix constructor #{symbolText @name}|]

instance (
    KnownSymbol name,
    DecodeRecord fields as
  ) => DecodeCtor ('Record name fields) as where
    decodeCtor = \case
      Msgpack fields ->
        decodeRecord @fields @as fields
      ObjectMap _ ->
        incompatibleShape desc "Map with non-string keys"
      o ->
        incompatibleCon desc o
      where
        desc =
          [exon|record constructor #{symbolText @name}|]

class DecodeCtors (ctors :: [ConstructorInfo]) (ass :: [[Type]]) where
  decodeCtors :: Object -> Either FieldError (NS (NP I) ass)

instance (
    DecodeCtor ctor as
  ) => DecodeCtors '[ctor] '[as] where
  decodeCtors o =
    Z <$> decodeCtor @ctor @as o

instance (
    DecodeCtor ctor as,
    DecodeCtors (ctor1 : ctors) (as1 : ass)
  ) => DecodeCtors (ctor : ctor1 : ctors) (as : as1 : ass) where
    decodeCtors o =
      either (const (S <$> decodeCtors @(ctor1 : ctors) @(as1 : ass) o)) (Right . Z) (decodeCtor @ctor @as o)

instance (
    MsgpackDecode a
  ) => GMsgpackDecode ('Newtype mod name ctor) '[ '[a]] where
    gfromMsgpack o = do
      a <- nestedDecode o
      pure (SOP (Z (I a :* Nil)))

instance (
    DecodeCtors ctors ass
  ) => GMsgpackDecode ('ADT mod name ctors strictness) ass where
  gfromMsgpack =
      fmap SOP . decodeCtors @ctors @ass

instance (
    Ord k,
    Typeable k,
    Typeable v,
    MsgpackDecode k,
    MsgpackDecode v
  ) => MsgpackDecode (Map k v) where
    fromMsgpack = \case
      ObjectMap om -> do
        Map.fromList <$> traverse decodePair (Map.toList om)
        where
          decodePair (k, v) = do
            k1 <- fromMsgpack k
            v1 <- fromMsgpack v
            pure (k1, v1)
      o ->
        toDecodeError (incompatible o)

instance MsgpackDecode Integer where
  fromMsgpack =
    decodeIntegral

instance MsgpackDecode Int where
  fromMsgpack =
    decodeIntegral

instance MsgpackDecode Int64 where
  fromMsgpack =
    decodeIntegral

instance MsgpackDecode Float where
  fromMsgpack =
    decodeFractional

instance MsgpackDecode Double where
  fromMsgpack =
    decodeFractional

instance {-# overlapping #-} MsgpackDecode String where
  fromMsgpack =
    decodeUtf8Lenient

instance {-# overlappable #-} (
    Typeable a,
    MsgpackDecode a
  ) => MsgpackDecode [a] where
    fromMsgpack = \case
      ObjectArray oa ->
        traverse fromMsgpack oa
      o ->
        decodeIncompatible o

instance MsgpackDecode Text where
  fromMsgpack =
    decodeUtf8Lenient

instance MsgpackDecode ValidUtf8 where
  fromMsgpack =
    decodeByteString (bimap utf8Error ValidUtf8 . decodeUtf8Strict)

instance MsgpackDecode ValidUtf8String where
  fromMsgpack =
    decodeByteString (bimap utf8Error ValidUtf8String . decodeUtf8Strict)

instance MsgpackDecode ByteString where
  fromMsgpack =
    decodeByteString Right

instance MsgpackDecode Char where
  fromMsgpack o =
    decodeByteString (check . decodeUtf8) o
    where
      check :: [Char] -> Either FieldError Char
      check = \case
        [c] ->
          Right c
        _ ->
          Left "Got multiple characters"

instance MsgpackDecode a => MsgpackDecode (Maybe a) where
  fromMsgpack = \case
    ObjectNil ->
      Right Nothing
    o ->
      Just <$> fromMsgpack o

instance (MsgpackDecode a, MsgpackDecode b) => MsgpackDecode (Either a b) where
  fromMsgpack o =
    fromRight (Left <$> fromMsgpack o) (Right . Right <$> fromMsgpack o)

instance MsgpackDecode Bool where
  fromMsgpack = \case
    ObjectBool a ->
      Right a
    ObjectInt 0 ->
      Right False
    ObjectInt 1 ->
      Right True
    o ->
      decodeIncompatible o

instance MsgpackDecode () where
  fromMsgpack _ =
    Right ()

instance MsgpackDecode Object where
  fromMsgpack =
    Right

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

decodePathWith ::
  ∀ p .
  (FilePath -> Either SomeException p) ->
  Object ->
  Either FieldError p
decodePathWith dec o = do
  ValidUtf8String s <- nestedDecode o
  first (const (FieldError "Invalid path")) (dec s)

decodePathE ::
  ∀ b t .
  DecodePath b t =>
  Object ->
  Either FieldError (Path b t)
decodePathE = decodePathWith decodePath

instance (
    Typeable b,
    Typeable t,
    DecodePath b t
  ) => MsgpackDecode (Path b t) where
    fromMsgpack =
      toDecodeError . decodePathE

instance MsgpackDecode (SomeBase File) where
  fromMsgpack = toDecodeError . decodePathWith parseSomeFile

instance MsgpackDecode (SomeBase Dir) where
  fromMsgpack = toDecodeError . decodePathWith parseSomeDir

timeUnit ::
  Typeable a =>
  Fractional a =>
  Object ->
  Either DecodeError a
timeUnit = \case
  Msgpack d ->
    Right (realToFrac @Double d)
  Msgpack i ->
    Right (fromIntegral @Int64 i)
  o ->
    decodeIncompatible o

instance MsgpackDecode NanoSeconds where
  fromMsgpack =
    timeUnit

instance MsgpackDecode MicroSeconds where
  fromMsgpack =
    timeUnit

instance MsgpackDecode MilliSeconds where
  fromMsgpack =
    timeUnit

instance MsgpackDecode Seconds where
  fromMsgpack =
    fmap Seconds . fromMsgpack

fromMsgpackText ::
  MsgpackDecode a =>
  Object ->
  Either Text a
fromMsgpackText =
  first renderError . fromMsgpack

-- |Pattern synonym for decoding an 'Object'.
pattern Msgpack :: ∀ a . MsgpackDecode a => a -> Object
pattern Msgpack a <- (fromMsgpack -> Right a)

deriving anyclass instance MsgpackDecode FieldError

deriving anyclass instance MsgpackDecode DecodeError

instance (
    Typeable a,
    Typeable b,
    MsgpackDecode a,
    MsgpackDecode b
  ) => MsgpackDecode (a, b)

instance (
    Typeable a,
    Typeable b,
    Typeable c,
    MsgpackDecode a,
    MsgpackDecode b,
    MsgpackDecode c
  ) => MsgpackDecode (a, b, c)

instance (
    Typeable a,
    Typeable b,
    Typeable c,
    Typeable d,
    MsgpackDecode a,
    MsgpackDecode b,
    MsgpackDecode c,
    MsgpackDecode d
  ) => MsgpackDecode (a, b, c, d)

instance (
    Typeable a,
    Typeable b,
    Typeable c,
    Typeable d,
    Typeable e,
    MsgpackDecode a,
    MsgpackDecode b,
    MsgpackDecode c,
    MsgpackDecode d,
    MsgpackDecode e
  ) => MsgpackDecode (a, b, c, d, e)

instance (
    Typeable a,
    Typeable b,
    Typeable c,
    Typeable d,
    Typeable e,
    Typeable f,
    MsgpackDecode a,
    MsgpackDecode b,
    MsgpackDecode c,
    MsgpackDecode d,
    MsgpackDecode e,
    MsgpackDecode f
  ) => MsgpackDecode (a, b, c, d, e, f)

instance (
    Typeable a,
    Typeable b,
    Typeable c,
    Typeable d,
    Typeable e,
    Typeable f,
    Typeable g,
    MsgpackDecode a,
    MsgpackDecode b,
    MsgpackDecode c,
    MsgpackDecode d,
    MsgpackDecode e,
    MsgpackDecode f,
    MsgpackDecode g
  ) => MsgpackDecode (a, b, c, d, e, f, g)
