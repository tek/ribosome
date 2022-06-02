module Main where

import Plugin.Test.PingTest (test_ping)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "ping" test_ping
  ]


main :: IO ()
main =
  defaultMain tests
