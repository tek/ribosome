module Main where

import Ribosome.Host.Test.BasicTest (test_basic)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Polysemy.Test (unitTest)

tests :: TestTree
tests =
  testGroup "host" [
    unitTest "basic" test_basic
  ]

main :: IO ()
main =
  defaultMain tests
