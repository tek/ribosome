module Ribosome.Data.Register where

import Data.Char (isAlpha, isNumber)
import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..), msgpackFromString)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (..))

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
  fromString = \case
    "" ->
      Empty
    [a] | isAlpha a ->
      Named (Text.singleton a)
    [a] | isNumber a ->
      Numbered (Text.singleton a)
    a ->
      Special (toText a)

instance MsgpackDecode Register where
  fromMsgpack =
    msgpackFromString "Register"

instance MsgpackEncode Register where
  toMsgpack (Named a) =
    toMsgpack a
  toMsgpack (Numbered a) =
    toMsgpack a
  toMsgpack (Special a) =
    toMsgpack a
  toMsgpack Empty =
    toMsgpack ("" :: Text)

registerRepr :: Register -> Text
registerRepr = \case
  Named a ->
    [exon|"#{a}|]
  Numbered a ->
    [exon|"#{a}|]
  Special a ->
    [exon|"#{a}|]
  Empty ->
    ""
