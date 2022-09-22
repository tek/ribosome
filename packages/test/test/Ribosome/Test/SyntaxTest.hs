module Ribosome.Test.SyntaxTest where

import Exon (exon)
import Polysemy.Test (UnitTest)

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Data.Syntax.Syntax (Syntax)
import Ribosome.Syntax.Build (build)
import Ribosome.Syntax.Dsl (hi, link, match, prefix, region, (#>), (>-))
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.SocketTmux (testSocketTmux)

colonsHi :: Map Text Text
colonsHi =
  [("cterm", "reverse"), ("ctermfg", "1"), ("gui", "reverse"), ("guifg", "#dc322f")]

syntax :: Syntax
syntax =
  build $ prefix "Test" $
  region "Signature" [exon|\v^\w+ ::|] "$" #> hi colonsHi (match "Colons" "::") >- link "Type" (match "Type" ".*")

test_syntax :: UnitTest
test_syntax =
  testSocketTmux do
    setCurrentBufferContent ["function :: String -> Int", "function _ = 5"]
    executeSyntax syntax
    awaitScreenshot False "syntax" 0
