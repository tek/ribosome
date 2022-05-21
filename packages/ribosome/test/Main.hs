module Main where

import Polysemy.Test (unitTest)
import Ribosome.Test.BufferTest (test_bufferForFile)
import Ribosome.Test.MappingTest (test_mapping)
import Ribosome.Test.MenuTest (test_menu)
import Ribosome.Test.ScratchTest (test_scratch)
import Ribosome.Test.WatcherTest (test_varWatcher)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    testGroup "buffer" [
      unitTest "buffer for file" test_bufferForFile
    ],
    test_scratch,
    unitTest "mapping" test_mapping,
    unitTest "variable watcher" test_varWatcher,
    test_menu
  ]

main :: IO ()
main =
  defaultMain tests
