module Ribosome.Host.Class.Msgpack.Encode where

import qualified Data.Map.Strict as Map
import Data.MessagePack (
  Object (ObjectArray, ObjectBool, ObjectDouble, ObjectFloat, ObjectInt, ObjectMap, ObjectNil, ObjectString),
  )
import Generics.SOP (All, I (I), K (K), NP (Nil, (:*)), NS (S, Z), SOP (SOP), hcmap, hcollapse, unI, unSOP)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, gfrom)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Record),
  DatatypeInfo (ADT, Newtype),
  FieldInfo (FieldInfo),
  )
import Path (Path, SomeBase, prjSomeBase, toFilePath)
import Time (
  MicroSeconds (unMicroSeconds),
  MilliSeconds (unMilliSeconds),
  NanoSeconds (unNanoSeconds),
  Seconds (unSeconds),
  )

import Ribosome.Host.Class.Msgpack.Error (DecodeError, FieldError)
import Ribosome.Host.Class.Msgpack.Util (ConstructSOP, ValidUtf8, encodeString, unValidUtf8)

class EncodeRecord (fields :: [FieldInfo]) (as :: [Type]) where
  encodeRecord :: NP I as -> [(Object, Object)]

instance EncodeRecord '[] '[] where
  encodeRecord Nil =
    []

instance (
    KnownSymbol name,
    MsgpackEncode a,
    EncodeRecord fields as
  ) => EncodeRecord ('FieldInfo name : fields) (a : as) where
    encodeRecord (I a :* fields) =
      (ObjectString (encodeUtf8 (symbolVal (Proxy @name))), toMsgpack a) : encodeRecord @fields fields

class EncodeCtor (ctor :: ConstructorInfo) (as :: [Type]) where
  encodeCtor :: NP I as -> Object

instance (
    All MsgpackEncode as
  ) => EncodeCtor ('Constructor name) as where
    encodeCtor ctor =
      ObjectArray (hcollapse (hcmap (Proxy @MsgpackEncode) (K . toMsgpack . unI) ctor))

instance (
    EncodeRecord fields as
  ) => EncodeCtor ('Record name fields) as where
    encodeCtor ctor =
      ObjectMap (Map.fromList (encodeRecord @fields ctor))

class EncodeCtors (ctors :: [ConstructorInfo]) (ass :: [[Type]]) where
  encodeCtors :: NS (NP I) ass -> Object

instance (
    EncodeCtor ctor as
  ) => EncodeCtors '[ctor] '[as] where
    encodeCtors = \case
      Z ctor -> encodeCtor @ctor ctor
      S ctors -> case ctors of

instance (
    EncodeCtor ctor as,
    EncodeCtors (ctor1 : ctors) ass
  ) => EncodeCtors (ctor : ctor1 : ctors) (as : ass) where
    encodeCtors = \case
      Z ctor -> encodeCtor @ctor ctor
      S ctors -> encodeCtors @(ctor1 : ctors) ctors

class GMsgpackEncode (dt :: DatatypeInfo) (ass :: [[Type]]) where
  gtoMsgpack :: SOP I ass -> Object

instance (
    EncodeCtors ctors ass
  ) => GMsgpackEncode ('ADT mod name ctors strictness) ass where
  gtoMsgpack =
      encodeCtors @ctors @ass . unSOP

instance (
    MsgpackEncode a
  ) => GMsgpackEncode ('Newtype mod name ctor) '[ '[a]] where
    gtoMsgpack (SOP (Z (I a :* Nil))) =
      toMsgpack a
    gtoMsgpack (SOP (S ns)) =
      case ns of

-- | Encode a value to a MessagePack 'Object' using its type's 'Generic' instance.
toMsgpackGeneric ::
  ∀ a ass .
  ConstructSOP a ass =>
  GMsgpackEncode (GDatatypeInfoOf a) (GCode a) =>
  a ->
  Object
toMsgpackGeneric =
  gtoMsgpack @(GDatatypeInfoOf a) . gfrom

-- |Class of values that can be encoded to MessagePack 'Object's.
class MsgpackEncode a where
  -- |Encode a value to MessagePack.
    --
  -- The default implementation uses generic derivation.
  toMsgpack :: a -> Object

  default toMsgpack ::
    ConstructSOP a ass =>
    GMsgpackEncode (GDatatypeInfoOf a) (GCode a) =>
    a ->
    Object
  toMsgpack = toMsgpackGeneric

instance (
    MsgpackEncode k,
    MsgpackEncode v
  ) => MsgpackEncode (Map k v) where
  toMsgpack = ObjectMap . Map.fromList . fmap (bimap toMsgpack toMsgpack) . Map.toList

instance MsgpackEncode Integer where
  toMsgpack =
    ObjectInt . fromInteger

instance MsgpackEncode Int where
  toMsgpack =
    ObjectInt . fromIntegral

instance MsgpackEncode Int64 where
  toMsgpack =
    ObjectInt

instance MsgpackEncode Float where
  toMsgpack =
    ObjectFloat

instance MsgpackEncode Double where
  toMsgpack =
    ObjectDouble

instance {-# overlapping #-} MsgpackEncode String where
  toMsgpack =
    encodeString

instance {-# overlappable #-} MsgpackEncode a => MsgpackEncode [a] where
  toMsgpack =
    ObjectArray . fmap toMsgpack

instance MsgpackEncode a => MsgpackEncode (NonEmpty a) where
  toMsgpack =
    toMsgpack . toList

instance MsgpackEncode a => MsgpackEncode (Seq a) where
  toMsgpack =
    toMsgpack . toList

instance MsgpackEncode Text where
  toMsgpack =
    encodeString

instance MsgpackEncode ValidUtf8 where
  toMsgpack =
    toMsgpack . (.unValidUtf8)

instance MsgpackEncode a => MsgpackEncode (Maybe a) where
  toMsgpack =
    maybe ObjectNil toMsgpack

instance (
    MsgpackEncode a,
    MsgpackEncode b
  ) => MsgpackEncode (Either a b) where
    toMsgpack =
      either toMsgpack toMsgpack

instance MsgpackEncode Bool where
  toMsgpack =
    ObjectBool

instance MsgpackEncode () where
  toMsgpack _ =
    ObjectNil

instance MsgpackEncode Object where
  toMsgpack =
    id

instance MsgpackEncode ByteString where
  toMsgpack =
    ObjectString

instance (MsgpackEncode a, MsgpackEncode b) => MsgpackEncode (a, b) where
  toMsgpack (a, b) =
    ObjectArray [toMsgpack a, toMsgpack b]

instance (MsgpackEncode a, MsgpackEncode b, MsgpackEncode c) => MsgpackEncode (a, b, c) where
  toMsgpack (a, b, c) =
    ObjectArray [toMsgpack a, toMsgpack b, toMsgpack c]

instance MsgpackEncode (Path b t) where
  toMsgpack =
    ObjectString . encodeUtf8 . toFilePath

instance MsgpackEncode (SomeBase t) where
  toMsgpack =
    ObjectString . encodeUtf8 . prjSomeBase toFilePath

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

deriving anyclass instance MsgpackEncode FieldError

deriving anyclass instance MsgpackEncode DecodeError
