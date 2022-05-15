module Ribosome.Test.MsgpackTest where

import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object (..))
import Hedgehog ((===))
import Path (Abs, Dir, File, Path, Rel, absfile, relfile)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (..), fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (..))
import qualified Ribosome.Msgpack.Util as Util (string)
import Ribosome.Test.Run (UnitTest, unitTest)

newtype NT =
  NT Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode)

data Blob =
  Blob {
    key4 :: [[Int]],
    key5 :: Map Int Text,
    key6 :: NT,
    key7 :: (Int, String)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

data Prod =
  Prod Text Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

data Dat =
  Dat {
    key1 :: Blob,
    key2 :: Bool,
    key3 :: Maybe Prod
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

dat :: Dat
dat = Dat (Blob [[1, 2], [3]] (Map.fromList [(1, "1"), (2, "2")]) (NT "nt") (91, "pair")) False (Just $ Prod "dat" 27)

os :: String -> Object
os = Util.string

i :: Int64 -> Object
i = ObjectInt

encodedBlob :: Object
encodedBlob =
  ObjectMap $ Map.fromList [
    (os "key4", ObjectArray [ObjectArray [i 1, i 2], ObjectArray [i 3]]),
    (os "key5", ObjectMap $ Map.fromList [(i 1, os "1"), (i 2, os "2")]),
    (os "key6", os "nt"),
    (os "key7", ObjectArray [ObjectInt 91, ObjectString "pair"])
    ]

encoded :: Object
encoded =
  ObjectMap $ Map.fromList [
    (os "key1", encodedBlob),
    (os "key2", ObjectBool False),
    (ObjectString "key3", ObjectArray [os "dat", i 27])
    ]

test_encode :: UnitTest
test_encode =
  encoded === toMsgpack dat

doc2Text :: Either (Doc AnsiStyle) a -> Either Text a
doc2Text =
  mapLeft (renderStrict . layoutPretty defaultLayoutOptions)

decodeSubject :: Object
decodeSubject =
  ObjectMap $ Map.fromList [
    (os "key1", encodedBlob),
    (os "key2", ObjectBool False),
    (ObjectBinary "key3", ObjectArray [os "dat", i 27])
    ]

test_decodeBasic :: UnitTest
test_decodeBasic =
  Right dat === doc2Text (fromMsgpack decodeSubject)

data Nope =
  Nope {
    present :: Int,
    absent :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode)

encodedNope :: Object
encodedNope =
  ObjectMap $ Map.fromList [(os "present", i 5)]

test_maybeMissing :: UnitTest
test_maybeMissing =
  Right (Nope 5 Nothing) === doc2Text (fromMsgpack encodedNope)

test_decodeEither :: UnitTest
test_decodeEither =
  Right (Left "text" :: Either Text Int) === doc2Text (fromMsgpack (ObjectBinary "text"))

data ST =
  STL {
    stName :: Text,
    stCount :: Int
  }
  |
  STR {
    stName :: Text,
    stDesc :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

sumName :: Text
sumName =
  "sumName"

sumCount :: Int
sumCount =
  1313

encodedSum :: Object
encodedSum =
  ObjectMap $ Map.fromList [
    (toMsgpack @Text "stName", toMsgpack sumName),
    (toMsgpack @Text "stCount", toMsgpack sumCount)
    ]

test_encodeSum :: UnitTest
test_encodeSum =
  encodedSum === toMsgpack (STL sumName sumCount)

test_decodeSum :: UnitTest
test_decodeSum =
  Right (STL sumName sumCount) === doc2Text (fromMsgpack encodedSum)

encodedPath :: Object
encodedPath =
  ObjectString "/path/to/file"

test_decodePath :: UnitTest
test_decodePath =
  Right [absfile|/path/to/file|] === doc2Text (fromMsgpack @(Path Abs File) encodedPath)

test_failDecodePath :: UnitTest
test_failDecodePath =
  Left "InvalidRelDir \"/path/to/file\"" === doc2Text (fromMsgpack @(Path Rel Dir) encodedPath)

test_encodePath :: UnitTest
test_encodePath =
  ObjectString "path/to/file" === toMsgpack [relfile|path/to/file|]

test_msgpack :: TestTree
test_msgpack =
  testGroup "msgpack" [
    unitTest "encode" test_encode,
    unitTest "decode" test_decodeBasic,
    unitTest "decode Nothing" test_maybeMissing,
    unitTest "decode Right" test_decodeEither,
    unitTest "encode sum type" test_encodeSum,
    unitTest "decode sum type" test_decodeSum,
    unitTest "decode Path" test_decodePath,
    unitTest "decode invalid Path" test_failDecodePath,
    unitTest "encode Path" test_encodePath
  ]
