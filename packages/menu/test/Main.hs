module Main where

import Polysemy.Test (unitTest)
import Ribosome.Menu.Test.FilterTest (test_filterFuzzy)
import Ribosome.Menu.Test.MenuTest (test_menu)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_menu,
    test_nvimMenu,
    unitTest "fuzzy filter" test_filterFuzzy
  ]

main :: IO ()
main =
  defaultMain tests
