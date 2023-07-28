module Main where

import Polysemy.Test (unitTest)
import Ribosome.Menu.Test.FilterTest (test_filterFuzzy)
import Ribosome.Menu.Test.MenuTest (test_menu)
import Ribosome.Menu.Test.MultilineTest (test_multiline, test_multilineCramped)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Ribosome.Menu.Test.SliceTest (test_slice)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_menu,
    test_nvimMenu,
    unitTest "fuzzy filter" test_filterFuzzy,
    test_slice,
    unitTest "multiline menu entries" test_multiline,
    unitTest "multiline with little space" test_multilineCramped
  ]

main :: IO ()
main =
  defaultMain tests
