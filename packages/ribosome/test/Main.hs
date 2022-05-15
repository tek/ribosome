module Main where

import Polysemy.Test (unitTest)
import Ribosome.Test.BufferTest (test_bufferForFile)
import Ribosome.Test.ScratchTest (test_scratch)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    testGroup "buffer" [
      unitTest "buffer for file" test_bufferForFile
    ],
    test_scratch
  ]

main :: IO ()
main =
  defaultMain tests
