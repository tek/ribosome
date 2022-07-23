module Ribosome.Test.SyntaxTest where

import Polysemy.Test (UnitTest)

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Syntax (Syntax (Syntax), syntaxHighlight, syntaxMatch)
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.SocketTmux (testSocketTmux)

syntax :: Syntax
syntax =
  Syntax [syntaxMatch "TestColons" "::"] [
    syntaxHighlight "TestColons" [("cterm", "reverse"), ("ctermfg", "1"), ("gui", "reverse"), ("guifg", "#dc322f")]
  ] []

test_syntax :: UnitTest
test_syntax =
  testSocketTmux do
    setCurrentBufferContent ["function :: String -> Int", "function _ = 5"]
    _ <- executeSyntax syntax
    awaitScreenshot False "syntax" 0
