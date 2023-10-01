module Main where

import Polysemy.Test (unitTest, unitTestTimes)
import Ribosome.Menu.Test.BottomStatusTest (test_bottomStatus)
import Ribosome.Menu.Test.CursorClampTest (test_clampCursor)
import Ribosome.Menu.Test.EditTest (test_editMode)
import Ribosome.Menu.Test.FilterTest (test_filterFuzzy)
import Ribosome.Menu.Test.BasicTest (test_basic)
import Ribosome.Menu.Test.MultilineTest (test_multiline, test_multilineCramped)
import Ribosome.Menu.Test.NativeInputTest (test_nativeInput)
import Ribosome.Menu.Test.NoMatchTest (test_filterNoMatch)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Ribosome.Menu.Test.SliceTest (test_slice)
import Ribosome.Test.Skip (requireX)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_basic,
    test_nvimMenu,
    test_nativeInput,
    requireX (unitTestTimes 3 "extra bottom status message") test_bottomStatus,
    unitTest "fuzzy filter" test_filterFuzzy,
    test_slice,
    unitTest "multiline menu entries" test_multiline,
    unitTest "multiline with little space" test_multilineCramped,
    unitTest "query with no match" test_filterNoMatch,
    unitTest "clamp cursor after refining" test_clampCursor,
    unitTest "use the menu as a kv editor" test_editMode
  ]

main :: IO ()
main =
  defaultMain tests
