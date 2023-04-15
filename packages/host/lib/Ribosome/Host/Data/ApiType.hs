module Ribosome.Host.Data.ApiType where

import Data.Char (isSpace)
import Exon (exon)
import qualified FlatParse.Basic as FlatParse
import FlatParse.Basic (
  Result (Err, Fail, OK),
  anyAsciiDecimalInt,
  branch,
  char,
  inSpan,
  isLatinLetter,
  optional,
  runParser,
  satisfy,
  string,
  switch,
  takeRest,
  withSpan,
  (<|>),
  )
import Prelude hiding (optional, some, span, try, (<|>))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Error (DecodeError, decodeError)

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

polyType :: ApiType -> Bool
polyType = \case
  Prim Object -> True
  Prim Dictionary -> True
  _ -> False

pattern PolyType :: ApiType
pattern PolyType <- (polyType -> True)

type Parser =
  FlatParse.Parser Text

ws :: Parser ()
ws =
  void (FlatParse.many (satisfy isSpace))

span :: Parser () -> Parser String
span seek =
  withSpan seek \ _ sp -> inSpan sp (decodeUtf8 <$> takeRest)

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
    anyAsciiDecimalInt
  pure (Array t arity)

array :: Parser ApiType
array = do
  $(string "Array")
  branch $(string "Of(") (typedArray <* $(char ')')) (pure (Array (Prim Object) Nothing))

ext :: Parser ApiType
ext =
  Ext <$> span (void (many (satisfy isLatinLetter)))

apiType :: Parser ApiType
apiType =
  array <|> (Prim <$> prim) <|> ext

parseApiType :: ByteString -> Either DecodeError ApiType
parseApiType =
  runParser apiType >>> \case
    OK a "" -> Right a
    OK a u -> decodeError [exon|Parsed #{toText (showsPrec 11 a "")} but got leftovers: #{decodeUtf8 u}|]
    Fail -> decodeError "fail"
    Err e -> decodeError e

instance MsgpackDecode ApiType where
  fromMsgpack =
    parseApiType <=< fromMsgpack
