module Main where

import Polysemy.Test (unitTest)
import Ribosome.App.Test.NewProjectTest (test_newProject)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    unitTest "create a new project" test_newProject
  ]

main :: IO ()
main =
  defaultMain tests
