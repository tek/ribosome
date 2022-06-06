module Main where

import Polysemy.Test (unitTest)
import Ribosome.Test.SyntaxTest (test_syntax)
import Ribosome.Test.TmuxTest (test_tmux)
import Skip (skipUnlessX)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    unitTest "tmux" (skipUnlessX test_tmux),
    unitTest "syntax" (skipUnlessX test_syntax)
  ]

main :: IO ()
main =
  defaultMain tests
