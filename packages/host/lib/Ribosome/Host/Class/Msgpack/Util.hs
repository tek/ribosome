{-# options_haddock prune #-}

-- |Utilities for writing messagepack codec instances.
module Ribosome.Host.Class.Msgpack.Util where

import Data.MessagePack (Object (..))
import Exon (exon)
import Generics.SOP (All2, Top)
import Generics.SOP.GGP (GCode, GFrom, GTo)
import Type.Reflection (typeRep)

import Ribosome.Host.Class.Msgpack.Error (
  DecodeError,
  FieldError (FieldError),
  incompatible,
  incompatibleCon,
  toDecodeError,
  )

type ReifySOP (a :: Type) (ass :: [[Type]]) =
  (Generic a, GTo a, GCode a ~ ass, All2 Top ass)

type ConstructSOP (a :: Type) (ass :: [[Type]]) =
  (Generic a, GFrom a, GCode a ~ ass, All2 Top ass)

newtype ValidUtf8 =
  ValidUtf8 { unValidUtf8 :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype ValidUtf8String =
  ValidUtf8String { unValidUtf8String :: String }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

maybeByteString :: Object -> Maybe ByteString
maybeByteString = \case
  ObjectString os ->
    Just os
  ObjectBinary os ->
    Just os
  _ ->
    Nothing

maybeString :: Object -> Maybe String
maybeString =
  fmap decodeUtf8 . maybeByteString

-- |Extract a 'String' from an 'Object'.
pattern MsgpackString :: String -> Object
pattern MsgpackString s <- (maybeString -> Just s)

-- |Call the continuation if the 'Object' contains a 'ByteString', or an error otherwise.
byteStringField ::
  Typeable a =>
  (ByteString -> Either FieldError a) ->
  Object ->
  Either FieldError a
byteStringField decode = \case
  ObjectString os ->
    decode os
  ObjectBinary os ->
    decode os
  o ->
    incompatible o

-- |Decode a 'ByteString' field using 'IsString'.
stringField ::
  Typeable a =>
  IsString a =>
  Object ->
  Either FieldError a
stringField =
  byteStringField (Right . fromString . decodeUtf8)

-- |Decode a 'ByteString' type using 'IsString'.
decodeString ::
  Typeable a =>
  IsString a =>
  Object ->
  Either DecodeError a
decodeString =
  toDecodeError . stringField

-- |Decode a 'ByteString' type using 'IsString'.
decodeByteString ::
  Typeable a =>
  (ByteString -> Either FieldError a) ->
  Object ->
  Either DecodeError a
decodeByteString f =
  toDecodeError . byteStringField f

-- |Decode a 'ByteString' type using 'ConvertUtf8'.
decodeUtf8Lenient ::
  Typeable a =>
  ConvertUtf8 a ByteString =>
  Object ->
  Either DecodeError a
decodeUtf8Lenient =
  decodeByteString (Right . decodeUtf8)

-- |Decode a 'ByteString' field using 'Read'.
readField ::
  ∀ a .
  Read a =>
  Typeable a =>
  String ->
  Either FieldError a
readField s =
  first err (readEither s)
  where
    err _ =
      FieldError [exon|Got #{toText s} for #{show (typeRep @a)}|]

-- |Decode a numeric or string field using 'Integral' or 'Read'.
integralField ::
  ∀ a .
  Read a =>
  Integral a =>
  Typeable a =>
  Object ->
  Either FieldError a
integralField = \case
  ObjectInt i ->
    Right (fromIntegral i)
  ObjectUInt i ->
    Right (fromIntegral i)
  MsgpackString s ->
    readField s
  o ->
    incompatible o

-- |Decode a numeric or string type using 'Integral' or 'Read'.
decodeIntegral ::
  ∀ a .
  Read a =>
  Integral a =>
  Typeable a =>
  Object ->
  Either DecodeError a
decodeIntegral =
  toDecodeError . integralField

-- |Decode a numeric or string field using 'Fractional' or 'Read'.
fractionalField ::
  Read a =>
  Typeable a =>
  Fractional a =>
  Object ->
  Either FieldError a
fractionalField = \case
  ObjectFloat a ->
    Right (realToFrac a)
  ObjectDouble a ->
    Right (realToFrac a)
  ObjectInt i ->
    Right (fromIntegral i)
  ObjectUInt i ->
    Right (fromIntegral i)
  MsgpackString s ->
    readField s
  o ->
    incompatible o

-- |Decode a numeric or string type using 'Fractional' or 'Read'.
decodeFractional ::
  ∀ a .
  Read a =>
  Fractional a =>
  Typeable a =>
  Object ->
  Either DecodeError a
decodeFractional =
  toDecodeError . fractionalField

withArray ::
  Text ->
  ([Object] -> Either FieldError a) ->
  Object ->
  Either FieldError a
withArray target f = \case
  ObjectArray elems ->
    f elems
  o ->
    incompatibleCon target o

encodeString ::
  ConvertUtf8 a ByteString =>
  a ->
  Object
encodeString =
  ObjectString . encodeUtf8

encodeBinary ::
  ConvertUtf8 a ByteString =>
  a ->
  Object
encodeBinary =
  ObjectBinary . encodeUtf8
