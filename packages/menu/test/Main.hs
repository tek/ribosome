module Main where

import Polysemy.Test (unitTest)
import Ribosome.Menu.Test.FilterTest (test_filterFuzzy)
import Ribosome.Menu.Test.MenuTest (test_basic)
import Ribosome.Menu.Test.MultilineTest (test_multiline, test_multilineCramped)
import Ribosome.Menu.Test.NoMatchTest (test_filterNoMatch)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Ribosome.Menu.Test.SliceTest (test_slice)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_basic,
    test_nvimMenu,
    unitTest "fuzzy filter" test_filterFuzzy,
    test_slice,
    unitTest "multiline menu entries" test_multiline,
    unitTest "multiline with little space" test_multilineCramped,
    unitTest "query with no match" test_filterNoMatch
  ]

main :: IO ()
main =
  defaultMain tests
