module Ribosome.Msgpack.Util where

import Data.Bifunctor (first)
import qualified Data.ByteString.UTF8 as ByteString (fromString)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object(..))
import Data.Text.Prettyprint.Doc (Doc, pretty, viaShow, (<+>))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

import Ribosome.Data.Maybe (orElse)

type Err = Doc AnsiStyle

string :: ConvertUtf8 a ByteString => a -> Object
string = ObjectString . encodeUtf8

binary :: ConvertUtf8 a ByteString => a -> Object
binary = ObjectBinary . encodeUtf8

text :: Text -> Object
text = ObjectString . encodeUtf8

assembleMap :: [(String, Object)] -> Object
assembleMap =
  ObjectMap . Map.fromList . (fmap . first) string

invalid :: Text -> Object -> Either Err a
invalid msg obj =
  Left $ pretty (msg <> ": ") <+> viaShow obj

missingRecordKey :: String -> Object -> Either Err a
missingRecordKey key =
  invalid $ "missing record key " <> toText key <> " in ObjectMap"

illegalType :: Text -> Object -> Either Err a
illegalType tpe =
  invalid $ "illegal type for " <> tpe

lookupObjectMap ::
  ConvertUtf8 a ByteString =>
  a ->
  Map Object Object ->
  Maybe Object
lookupObjectMap key o =
  (o !? string key) `orElse` (o !? binary key)
