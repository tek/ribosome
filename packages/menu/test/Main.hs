module Main where

import Ribosome.Menu.Test.MenuTest (test_menu)
import Test.Tasty (TestTree, defaultMain)

tests :: TestTree
tests =
  test_menu

main :: IO ()
main =
  defaultMain tests
