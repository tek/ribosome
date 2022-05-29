module Main where

import Integration.Test.PluginTest (test_plugin)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "plugin" test_plugin
  ]

main :: IO ()
main =
  defaultMain tests
