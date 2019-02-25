{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module MsgpackSpec(
  htf_thisModulesTests
) where

import Data.Either.Combinators (mapLeft)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Terminal (renderStrict, AnsiStyle)
import GHC.Generics (Generic)
import Test.Framework

import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import qualified Ribosome.Msgpack.Util as Util (string)

newtype NT =
  NT String
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Blob =
  Blob {
    key4 :: [[Int]],
    key5 :: Map Int String,
    key6 :: NT
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Prod =
  Prod String Int
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Dat =
  Dat {
    key1 :: Blob,
    key2 :: Bool,
    key3 :: Maybe Prod
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

dat :: Dat
dat = Dat (Blob [[1, 2], [3]] (Map.fromList [(1, "1"), (2, "2")]) (NT "nt")) False (Just $ Prod "dat" 27)

os :: String -> Object
os = Util.string

i :: Int64 -> Object
i = ObjectInt

encodedBlob :: Object
encodedBlob =
  ObjectMap $ Map.fromList [
    (os "key4", ObjectArray [ObjectArray [i 1, i 2], ObjectArray [i 3]]),
    (os "key5", ObjectMap $ Map.fromList [(i 1, os "1"), (i 2, os "2")]),
    (os "key6", os "nt")
    ]

encoded :: Object
encoded =
  ObjectMap $ Map.fromList [
    (os "key1", encodedBlob),
    (os "key2", ObjectBool False),
    (os "key3", ObjectArray [os "dat", i 27])
    ]

test_encode :: IO ()
test_encode =
  assertEqual encoded (toMsgpack dat)

doc2Text :: Either (Doc AnsiStyle) a -> Either Text a
doc2Text =
  mapLeft (renderStrict . layoutPretty defaultLayoutOptions)

test_decode :: IO ()
test_decode =
  assertEqual (Right dat) (doc2Text $ fromMsgpack encoded)

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
