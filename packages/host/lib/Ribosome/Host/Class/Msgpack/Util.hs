module Ribosome.Host.Class.Msgpack.Util where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object (..))
import Exon (exon)

string :: ConvertUtf8 a ByteString => a -> Object
string = ObjectString . encodeUtf8

binary :: ConvertUtf8 a ByteString => a -> Object
binary = ObjectBinary . encodeUtf8

text :: Text -> Object
text = ObjectString . encodeUtf8

assembleMap :: [(String, Object)] -> Object
assembleMap =
  ObjectMap . Map.fromList . (fmap . first) string

invalid :: Text -> Object -> Either Text a
invalid msg obj =
  Left [exon|#{msg}: #{show obj}|]

missingRecordKey :: String -> Object -> Either Text a
missingRecordKey key =
  invalid [exon|missing record key #{toText key} in ObjectMap|]

illegalType :: Text -> Object -> Either Text a
illegalType tpe =
  invalid [exon|illegal type for #{tpe}|]

lookupObjectMap ::
  ConvertUtf8 a ByteString =>
  a ->
  Map Object Object ->
  Maybe Object
lookupObjectMap key o =
  (o !? string key) <|> (o !? binary key)
