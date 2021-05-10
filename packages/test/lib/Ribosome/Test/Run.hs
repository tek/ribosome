module Ribosome.Test.Run where

import Hedgehog (TestT, property, test, withTests)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

type UnitTest = TestT IO ()

unitTest ::
  TestName ->
  UnitTest ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test
