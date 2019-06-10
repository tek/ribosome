{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module MsgpackSpec(htf_thisModulesTests) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, renderStrict)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, Rel, absfile, relfile)
import Test.Framework

import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import qualified Ribosome.Msgpack.Util as Util (string)

newtype NT =
  NT Text
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Blob =
  Blob {
    key4 :: [[Int]],
    key5 :: Map Int Text,
    key6 :: NT,
    key7 :: (Int, String)
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Prod =
  Prod Text Int
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Dat =
  Dat {
    key1 :: Blob,
    key2 :: Bool,
    key3 :: Maybe Prod
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

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

test_encode :: IO ()
test_encode =
  assertEqual encoded (toMsgpack dat)

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

test_decodeBasic :: IO ()
test_decodeBasic =
  assertEqual (Right dat) (doc2Text $ fromMsgpack decodeSubject)

data Nope =
  Nope {
    present :: Int,
    absent :: Maybe Int
  }
  deriving (Eq, Show, Generic, MsgpackDecode)

encodedNope :: Object
encodedNope =
  ObjectMap $ Map.fromList [(os "present", i 5)]

test_maybeMissing :: IO ()
test_maybeMissing =
  assertEqual (Right (Nope 5 Nothing)) (doc2Text $ fromMsgpack encodedNope)

test_decodeEither :: IO ()
test_decodeEither =
  assertEqual (Right (Left "text" :: Either Text Int)) (doc2Text $ fromMsgpack $ ObjectBinary "text")

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
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

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

test_encodeSum :: IO ()
test_encodeSum =
  assertEqual encodedSum (toMsgpack (STL sumName sumCount))

test_decodeSum :: IO ()
test_decodeSum =
  assertEqual (Right $ STL sumName sumCount) (doc2Text $ fromMsgpack encodedSum)

encodedPath :: Object
encodedPath =
  ObjectString "/path/to/file"

test_decodePath :: IO ()
test_decodePath =
  assertEqual (Right [absfile|/path/to/file|]) (doc2Text $ fromMsgpack encodedPath)

test_failDecodePath :: IO ()
test_failDecodePath =
  assertEqual (Left "InvalidRelDir \"/path/to/file\"") (doc2Text $ fromMsgpack @(Path Rel Dir) encodedPath)

test_encodePath :: IO ()
test_encodePath =
  assertEqual (ObjectString "path/to/file") (toMsgpack [relfile|path/to/file|])
