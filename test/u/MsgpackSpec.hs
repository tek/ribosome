{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MsgpackSpec(
  htf_thisModulesTests
) where

import GHC.Generics (Generic)
import Data.Either.Combinators (mapLeft)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.MessagePack (Object(..))
import Test.Framework
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import qualified Ribosome.Msgpack.Util as Util (string)

data Blob =
  Blob {
    key4 :: [[Int]],
    key5 :: Map Int String
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Prod =
  Prod String Int
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

data Dat =
  Dat {
    key1 :: Blob,
    key2 :: Int,
    key3 :: Prod
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)

dat :: Dat
dat = Dat (Blob [[1, 2], [3]] (Map.fromList [(1, "1"), (2, "2")])) 13 (Prod "dat" 27)

os :: String -> Object
os = Util.string

i :: Int64 -> Object
i = ObjectInt

encodedBlob :: Object
encodedBlob =
  ObjectMap $ Map.fromList [
    (os "key4", ObjectArray [ObjectArray [i 1, i 2], ObjectArray [i 3]]),
    (os "key5", ObjectMap $ Map.fromList [(i 1, os "1"), (i 2, os "2")])
    ]

encoded :: Object
encoded =
  ObjectMap $ Map.fromList [(os "key1", encodedBlob), (os "key2", i 13), (os "key3", ObjectArray [os "dat", i 27])]

test_encode :: IO ()
test_encode =
  assertEqual encoded (toMsgpack dat)

test_decode :: IO ()
test_decode =
  assertEqual (Right dat) (mapLeft (const ()) $ fromMsgpack encoded)
