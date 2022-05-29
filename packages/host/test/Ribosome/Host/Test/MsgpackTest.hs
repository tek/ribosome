module Ribosome.Host.Test.MsgpackTest where

-- import Data.MessagePack (Object (ObjectMap, ObjectInt, ObjectArray, ObjectString, ObjectFloat))
-- import Polysemy.Test (UnitTest, runTestAuto, assertRight)
-- import Ribosome.Host.Class.Msgpack.DecodeSOP (fromMsgpack, MsgpackDecode)

-- data B =
--   B Double Int64 Object
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (MsgpackDecode)

-- data A =
--   A1 {
--     a :: Int,
--     b :: B
--   }
--   |
--   A2 B Double
--   |
--   A3 {
--     c :: Double
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (MsgpackDecode)

-- test_msgpack :: UnitTest
-- test_msgpack =
--   runTestAuto do
--     assertRight a1 (fromMsgpack (ObjectMap [(ObjectString "a", ObjectInt 5), (ObjectString "b", ObjectArray [ObjectFloat 23.23, ObjectInt 13, o])]))
--   where
--     a1 =
--       A1 5 (B 23.23 13 o)
--     o =
--       ObjectArray [ObjectInt 100, ObjectInt 101]
