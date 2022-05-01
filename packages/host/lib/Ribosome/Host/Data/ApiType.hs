module Ribosome.Host.Data.ApiType where

import Data.Char (isSpace)
import Exon (exon)
import qualified FlatParse.Basic as FlatParse
import FlatParse.Basic (
  Result (Err, Fail, OK),
  branch,
  char,
  inSpan,
  isLatinLetter,
  many_,
  optional,
  readInt,
  runParser,
  satisfy,
  satisfyASCII,
  spanned,
  string,
  switch,
  takeRest,
  (<|>),
  )
import Prelude hiding (optional, some, span, try, (<|>))
import Text.Show (showsPrec)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))

-- TODO see if using GADT can move some TH stuff to type level
data ApiPrim =
  Boolean
  |
  Integer
  |
  Float
  |
  String
  |
  Dictionary
  |
  Object
  |
  Void
  |
  LuaRef
  deriving stock (Eq, Show)

data ApiType =
  Prim ApiPrim
  |
  Array ApiType (Maybe Int)
  |
  Ext String
  deriving stock (Show, Eq)

type Parser =
  FlatParse.Parser Text

ws :: Parser ()
ws =
  many_ (satisfy isSpace)

span :: Parser () -> Parser String
span seek =
  spanned seek \ _ sp -> inSpan sp takeRest

prim :: Parser ApiPrim
prim =
  $(switch [|
  case _ of
    "Boolean" -> pure Boolean
    "Integer" -> pure Integer
    "Float" -> pure Float
    "String" -> pure String
    "Dictionary" -> pure Dictionary
    "Object" -> pure Object
    "void" -> pure Void
    "LuaRef" -> pure LuaRef
  |])

typedArray :: Parser ApiType
typedArray = do
  t <- apiType
  arity <- optional do
    $(char ',')
    ws
    readInt
  pure (Array t arity)

array :: Parser ApiType
array = do
  $(string "Array")
  branch $(string "Of(") (typedArray <* $(char ')')) (pure (Array (Prim Object) Nothing))

ext :: Parser ApiType
ext =
  Ext <$> span (many_ (satisfyASCII isLatinLetter))

apiType :: Parser ApiType
apiType =
  array <|> (Prim <$> prim) <|> ext

parseApiType :: ByteString -> Either Text ApiType
parseApiType =
  runParser apiType >>> \case
    OK a "" -> Right a
    OK a u -> Left [exon|Parsed #{toText (showsPrec 11 a "")} but got leftovers: #{decodeUtf8 u}|]
    Fail -> Left "fail"
    Err e -> Left e

instance MsgpackDecode ApiType where
  fromMsgpack =
    parseApiType <=< fromMsgpack
