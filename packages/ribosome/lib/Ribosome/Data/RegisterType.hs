module Ribosome.Data.RegisterType where

import Prettyprinter (Pretty(..))

import Ribosome.Msgpack.Decode (MsgpackDecode(..), msgpackFromString)
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

data RegisterType =
  Character
  |
  Line
  |
  Block
  |
  BlockWidth Int
  |
  Unknown Text
  deriving stock (Eq, Show)

instance IsString RegisterType where
  fromString "v" =
    Character
  fromString "V" =
    Line
  fromString a@('c' : 'v' : _) =
    Unknown (toText a)
  fromString a =
    Unknown (toText a)

instance MsgpackDecode RegisterType where
  fromMsgpack = msgpackFromString "RegisterType"

instance MsgpackEncode RegisterType where
  toMsgpack Character =
    toMsgpack ("v" :: Text)
  toMsgpack Line =
    toMsgpack ("V" :: Text)
  toMsgpack Block =
    toMsgpack ("b" :: Text)
  toMsgpack (BlockWidth width) =
    toMsgpack ("b" <> show width :: Text)
  toMsgpack (Unknown _) =
    toMsgpack ("" :: Text)

instance Pretty RegisterType where
  pretty Character = "c"
  pretty Line = "v"
  pretty Block = "<c-v>"
  pretty (BlockWidth width) = "<c-v>" <> pretty width
  pretty (Unknown a) = pretty a
