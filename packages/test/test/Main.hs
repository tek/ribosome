module Main where

import Polysemy.Test (unitTest)
import Ribosome.Test.EmbedTmuxTest (test_embedTmux)
import Ribosome.Test.SocketTmuxTest (test_socketTmux)
import Ribosome.Test.SyntaxTest (test_syntax)
import Skip (skipUnlessX)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ribosome" [
    unitTest "socket tmux" (skipUnlessX test_socketTmux),
    unitTest "embed tmux" (skipUnlessX test_embedTmux),
    unitTest "syntax" (skipUnlessX test_syntax)
  ]

main :: IO ()
main =
  defaultMain tests
