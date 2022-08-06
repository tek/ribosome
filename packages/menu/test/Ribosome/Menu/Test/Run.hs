module Ribosome.Menu.Test.Run where

import Hedgehog (TestLimit, property, test, withTests)
import Polysemy.Test (UnitTest)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

unitTestTimes ::
  TestLimit ->
  TestName ->
  UnitTest ->
  TestTree
unitTestTimes n desc =
  testProperty desc . withTests n . property . test
