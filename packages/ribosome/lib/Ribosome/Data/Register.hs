-- |Data types for register-related API functions.
module Ribosome.Data.Register where

import Data.Char (isAlpha, isNumber)
import qualified Data.Text as Text
import Exon (exon)
import Prettyprinter (Pretty (pretty))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (..))
import Ribosome.Host.Class.Msgpack.Util (decodeString)

-- |A Neovim register.
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
    decodeString

instance MsgpackEncode Register where
  toMsgpack (Named a) =
    toMsgpack a
  toMsgpack (Numbered a) =
    toMsgpack a
  toMsgpack (Special a) =
    toMsgpack a
  toMsgpack Empty =
    toMsgpack ("" :: Text)

-- |Render a register name by prefixing it with @"@.
quoted :: Text -> Text
quoted a =
  [exon|"#{a}|]

-- |Render a register name as is usual for Neovim.
registerRepr :: Register -> Text
registerRepr = \case
  Named a ->
    quoted a
  Numbered a ->
    quoted a
  Special a ->
    quoted a
  Empty ->
    ""

instance Pretty Register where
  pretty = \case
    Empty ->
      "no register"
    a ->
      pretty (registerRepr a)
