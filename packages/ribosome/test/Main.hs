module Main where

import Polysemy.Test (unitTest)
import Ribosome.Test.Buffer (test_bufferForFile)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    testGroup "buffer" [
      unitTest "buffer for file" test_bufferForFile
    ]
  ]

main :: IO ()
main =
  defaultMain tests
