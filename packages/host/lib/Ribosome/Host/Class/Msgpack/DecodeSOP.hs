module Ribosome.Host.Class.Msgpack.DecodeSOP where

import Data.MessagePack (Object (..))
import Generics.SOP (All2, I (I), NP (Nil, (:*)), NS (Z), SOP (SOP), Top)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, GFrom, GTo, gto)
import Generics.SOP.Type.Metadata (ConstructorInfo, DatatypeInfo (ADT, Newtype))

type ReifySOP (d :: Type) (dss :: [[Type]]) =
  (Generic d, GTo d, GCode d ~ dss, All2 Top dss)

type ConstructSOP (d :: Type) (dss :: [[Type]]) =
  (Generic d, GFrom d, GCode d ~ dss, All2 Top dss)

class MsgpackCtor (ctor :: ConstructorInfo) (as :: [Type]) where

class MsgpackCtors (ctors :: [ConstructorInfo]) (ass :: [[Type]]) where
  msgpackCtors :: Object -> Either Text (SOP I ass)

class GMsgpackDecode (dt :: DatatypeInfo) (ass :: [[Type]]) where
  gMsgpackDecode :: Object -> Either Text (SOP I ass)

instance (
    MsgpackDecode a
  ) => GMsgpackDecode ('Newtype mod name ctor) '[ '[a]] where
    gMsgpackDecode o = do
      a <- fromMsgpack o
      pure (SOP (Z (I a :* Nil)))

instance (
    MsgpackCtors ctors ass
  ) => GMsgpackDecode ('ADT mod name ctors strictness) ass where
  gMsgpackDecode =
      msgpackCtors @ctors

class MsgpackDecode a where
  fromMsgpack :: Object -> Either Text a
  default fromMsgpack ::
    ConstructSOP a ass =>
    ReifySOP a ass =>
    GMsgpackDecode (GDatatypeInfoOf a) (GCode a) =>
    Object ->
    Either Text a
  fromMsgpack =
    fmap gto . gMsgpackDecode @(GDatatypeInfoOf a)

  -- missingKey :: String -> Object -> Either Text a
  -- missingKey = Util.missingRecordKey

pattern Msgpack :: ∀ a . MsgpackDecode a => a -> Object
pattern Msgpack a <- (fromMsgpack -> Right a)
