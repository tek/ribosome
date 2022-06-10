module Main where

import Polysemy.Test (unitTest)
import Ribosome.Menu.Test.MenuTest (test_menu)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Ribosome.Menu.Test.PromptTest (test_promptSet)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_menu,
    test_nvimMenu,
    unitTest "" test_promptSet
  ]

main :: IO ()
main =
  defaultMain tests
