-- |Codec data type for Neovim register types.
module Ribosome.Data.RegisterType where

import Prettyprinter (Pretty (pretty))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (..))
import Ribosome.Host.Class.Msgpack.Util (decodeString)

-- |The type of a Neovim register, corresponding to concepts like line- or character-wise visual mode.
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
    decodeString

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
