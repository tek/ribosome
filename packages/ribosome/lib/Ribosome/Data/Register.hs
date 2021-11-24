module Ribosome.Data.Register where

import Data.Char (isAlpha, isNumber)
import qualified Data.Text as Text (singleton)
import Prettyprinter (Doc, Pretty(..))

import Ribosome.Msgpack.Decode (MsgpackDecode(..), msgpackFromString)
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

data Register =
  Named Text
  |
  Numbered Text
  |
  Special Text
  |
  Empty
  deriving stock (Eq, Show, Generic)

instance IsString Register where
  fromString "" =
    Empty
  fromString [a] | isAlpha a =
    Named $ Text.singleton a
  fromString [a] | isNumber a =
    Numbered $ Text.singleton a
  fromString a =
    Special $ toText a

instance MsgpackDecode Register where
  fromMsgpack = msgpackFromString "Register"

instance MsgpackEncode Register where
  toMsgpack (Named a) =
    toMsgpack a
  toMsgpack (Numbered a) =
    toMsgpack a
  toMsgpack (Special a) =
    toMsgpack a
  toMsgpack Empty =
    toMsgpack ("" :: Text)

prettyRegister :: Text -> Doc a
prettyRegister a =
  "\"" <> pretty a

instance Pretty Register where
  pretty (Named a) = prettyRegister a
  pretty (Numbered a) = prettyRegister (show a)
  pretty (Special a) = prettyRegister a
  pretty Empty = "no register"

registerRepr :: Register -> Text
registerRepr (Named a) =
  "\"" <> a
registerRepr (Numbered a) =
  "\"" <> a
registerRepr (Special a) =
  "\"" <> a
registerRepr Empty =
  ""
