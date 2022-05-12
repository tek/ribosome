module Main where

import Polysemy.Test (unitTest)
import Ribosome.Host.Test.PluginTest (test_plugin)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "plugin" test_plugin
  ]

main :: IO ()
main =
  defaultMain tests
