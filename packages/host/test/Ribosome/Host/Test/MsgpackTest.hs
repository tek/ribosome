module Ribosome.Host.Test.MsgpackTest where

import Data.MessagePack (Object (..))
import Polysemy.Test (UnitTest, assertLeft, assertRight, runTestAuto, unitTest, (===), Hedgehog)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode, fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode, toMsgpack)
import Ribosome.Host.Class.Msgpack.Error (DecodeError (DecodeError), FieldError (NestedFieldError), renderError)
import Ribosome.Host.Class.Msgpack.Util (ValidUtf8)

assertMsg ::
  ∀ a r .
  Show a =>
  HasCallStack =>
  MsgpackDecode a =>
  Member (Hedgehog IO) r =>
  Text ->
  Object ->
  Sem r ()
assertMsg err o =
  withFrozenCallStack do
    assertLeft @a err (first renderError (fromMsgpack o))

dec ::
  ∀ a r .
  Eq a =>
  Show a =>
  HasCallStack =>
  MsgpackDecode a =>
  Member (Hedgehog IO) r =>
  a ->
  Object ->
  Sem r ()
dec t d =
  withFrozenCallStack do
    assertRight t (fromMsgpack d)

data B =
  B Double Int64 Object
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

data A =
  A1 {
    a :: Int,
    b :: B
  }
  |
  A2 B Double
  |
  A3 {
    c :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

data C =
  C {
    a :: Maybe Int,
    b :: B
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

data T =
  T {
    t :: ValidUtf8
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

data Nest1 =
  Nest1 {
    n1 :: C
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

data Nest2 =
  Nest2 {
    n2 :: Nest1
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

o1 :: Object
o1 =
  ObjectArray [ObjectInt 100, ObjectInt 101]

b1 :: B
b1 =
  B 23.23 13 o1

cO :: Object
cO =
  ObjectMap [(ObjectString "b", bO)]

c1 :: C
c1 =
  C Nothing b1

tooManyError :: DecodeError
tooManyError =
  DecodeError "B" "Got 1 extra elements for product type"

missingError :: DecodeError
missingError =
  DecodeError "C" "Missing record field 'b'"

bO :: Object
bO =
  bOWith []

a1 :: A
a1 =
  A1 5 b1

a1O :: Object
a1O =
  ObjectMap [(ObjectString "a", ObjectInt 5), (ObjectString "b", bO)]

bOWith :: [Object] -> Object
bOWith extra =
  ObjectArray ([ObjectDouble 23.23, ObjectInt 13, o1] <> extra)

badUtf8 :: ByteString
badUtf8 =
  "\208\208\176\209\130\208\176\208\186"

utf8Error :: DecodeError
utf8Error =
  DecodeError "T" (NestedFieldError (DecodeError "ValidUtf8" "Invalid byte \\xd0"))

nestError :: Text
nestError =
  "Decoding C within Nest1 within Nest2: Missing record field 'b'"

tup5 :: (Int, String, Text, Bool, Maybe Double)
tup5 =
  (5, "1", "2", False, Just 3.3)

tup5O :: Object
tup5O =
    ObjectArray [ObjectInt 5, ObjectString "1", ObjectString "2", ObjectBool False, ObjectDouble 3.3]

test_msgpackDecode :: UnitTest
test_msgpackDecode =
  runTestAuto do
    dec b1 bO
    dec a1 a1O
    dec c1 cO
    dec (A3 1.1) (ObjectMap [(ObjectString "c", ObjectString "1.1")])
    dec @Text "\65533\1072\1090\1072\1082" (ObjectBinary badUtf8)
    dec tup5 tup5O
    assertLeft @B tooManyError (fromMsgpack (bOWith [ObjectNil]))
    assertLeft @C missingError (fromMsgpack missingO)
    assertLeft @T utf8Error (fromMsgpack (ObjectMap [(ObjectString "t", ObjectBinary badUtf8)]))
    assertMsg @Nest2 nestError nestO
    assertMsg @String "Decoding [Char]: Got Nil for [Char]" ObjectNil
  where
    missingO =
      ObjectMap [(ObjectString "a", ObjectInt 5)]
    nestO =
      ObjectMap [(ObjectString "n2", ObjectMap [(ObjectString "n1", missingO)])]

test_msgpackEncode :: UnitTest
test_msgpackEncode =
  runTestAuto do
    bO === toMsgpack b1
    a1O === toMsgpack a1
    ObjectMap [(ObjectString "b", bO), (ObjectString "a", ObjectNil)] === toMsgpack c1

test_msgpack :: TestTree
test_msgpack =
  testGroup "msgpack" [
    unitTest "decode" test_msgpackDecode,
    unitTest "encode" test_msgpackEncode
  ]
