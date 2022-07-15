module Ribosome.App.Templates.TestMainHs where

import Exon (exon)

import Ribosome.App.Data (ModuleName (ModuleName))

testMainHs :: ModuleName -> Text
testMainHs (ModuleName modName) =
  [exon|module Main where

import #{modName}.Test.PingTest (test_ping)
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
|]
