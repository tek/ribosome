module Main where

import Ribosome.Test.AutocmdTest (test_autocmd)
import Ribosome.Test.MappingTest (test_mapping)
import Ribosome.Test.MenuTest (test_menu)
import Ribosome.Test.MsgpackTest (test_msgpack)
import Ribosome.Test.NvimMenuTest (test_nvimMenu)
import Ribosome.Test.PromptTest (test_promptSet)
import Ribosome.Test.RpcTest (test_rpc)
import Ribosome.Test.Run (unitTest)
import Ribosome.Test.ScratchTest (test_floatScratch, test_regularScratch)
import Ribosome.Test.SettingTest (test_settingFail, test_settingSuccess)
import Ribosome.Test.StreamTest (test_mapMAcc)
-- import Ribosome.Test.SyntaxTest (test_syntax)
import Ribosome.Test.THTest ()
import Ribosome.Test.WatcherTest (test_varWatcher)
import Ribosome.Test.WindowTest (test_findMainWindowCreate, test_findMainWindowExisting)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "autocmd handler" test_autocmd,
    unitTest "mapping for scratch buffer" test_mapping,
    test_menu,
    test_msgpack,
    test_nvimMenu,
    unitTest "set the prompt text" test_promptSet,
    unitTest "rpc handlers" test_rpc,
    unitTest "scratch buffer" test_regularScratch,
    unitTest "floating scratch buffer" test_floatScratch,
    unitTest "successfully read a setting" test_settingSuccess,
    unitTest "read an unset setting" test_settingFail,
    -- TODO flaky test
    -- unitTest "syntax" test_syntax,
    unitTest "variable watcher" test_varWatcher,
    unitTest "find the existing main window" test_findMainWindowExisting,
    unitTest "create the main window" test_findMainWindowCreate,
    unitTest "mapMAcc stream combinator" test_mapMAcc
  ]

main :: IO ()
main =
  defaultMain tests
