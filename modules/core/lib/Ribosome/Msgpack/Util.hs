module Ribosome.Msgpack.Util where

import Data.Bifunctor (first)
import qualified Data.ByteString.UTF8 as ByteString (fromString)
import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object(..))
import Data.Text.Prettyprint.Doc (Doc, pretty, viaShow, (<+>))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

type Err = Doc AnsiStyle

string :: String -> Object
string = ObjectString . ByteString.fromString

text :: Text -> Object
text = ObjectString . encodeUtf8

assembleMap :: [(String, Object)] -> Object
assembleMap =
  ObjectMap . Map.fromList . (fmap . first) string

invalid :: String -> Object -> Either Err a
invalid msg obj =
  Left $ pretty (msg ++ ": ") <+> viaShow obj

missingRecordKey :: String -> Object -> Either Err a
missingRecordKey key =
  invalid $ "missing record key " ++ key ++ " in ObjectMap"

illegalType :: String -> Object -> Either Err a
illegalType tpe =
  invalid $ "illegal type for " ++ tpe
