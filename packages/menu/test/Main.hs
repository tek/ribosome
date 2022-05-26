module Main where

import Ribosome.Menu.Test.MenuTest (test_menu)
import Ribosome.Menu.Test.NvimMenuTest (test_nvimMenu)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "menu" [
    test_menu,
    test_nvimMenu
  ]

main :: IO ()
main =
  defaultMain tests
