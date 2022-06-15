module Ribosome.Data.RegisterType where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..), msgpackFromString)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (..))
import Prettyprinter (Pretty (pretty))

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
  deriving stock (Eq, Show, Ord)

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
  fromMsgpack =
    msgpackFromString "RegisterType"

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
  pretty = \case
    Character ->
      "c"
    Line ->
      "v"
    Block ->
      "<c-v>"
    BlockWidth width ->
      "<c-v>" <> pretty width
    Unknown a ->
      pretty a
