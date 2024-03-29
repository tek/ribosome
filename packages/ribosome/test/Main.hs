module Main where

import Polysemy.Test (unitTest)
import Ribosome.Test.BufferTest (test_bufferForFile)
import Ribosome.Test.MappingTest (test_mapping)
import Ribosome.Test.PersistTest (test_persist)
import Ribosome.Test.ScratchTest (test_scratch)
import Ribosome.Test.SettingTest (test_settings)
import Ribosome.Test.UndoTest (test_undo)
import Ribosome.Test.VariableTest (test_variable)
import Ribosome.Test.WatcherTest (test_varWatcher)
import Ribosome.Test.WindowTest (test_window)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    unitTest "buffer for file" test_bufferForFile,
    test_scratch,
    unitTest "mapping" test_mapping,
    unitTest "variable watcher" test_varWatcher,
    unitTest "persist" test_persist,
    test_window,
    test_settings,
    unitTest "undo" test_undo,
    test_variable
  ]

main :: IO ()
main =
  defaultMain tests
