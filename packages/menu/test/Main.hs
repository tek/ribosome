module Main where

import Polysemy.Test (unitTest)
import Ribosome.Menu.Test.MenuTest (test_menu)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Ribosome.Menu.Test.PromptTest (test_promptSet)
import Ribosome.Menu.Test.StreamTest (test_mapMAcc)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_menu,
    test_nvimMenu,
    unitTest "" test_promptSet,
    unitTest "mapMAcc" test_mapMAcc
  ]

main :: IO ()
main =
  defaultMain tests
