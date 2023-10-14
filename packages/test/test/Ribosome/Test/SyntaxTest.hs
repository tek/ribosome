module Ribosome.Test.SyntaxTest where

import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Test (UnitTest, (===))

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Data.Syntax.Syntax (Syntax)
import Ribosome.Internal.Syntax (syntaxCmdlines)
import Ribosome.Syntax.Build (build)
import Ribosome.Syntax.Dsl (hi, link, match, prefix, region, (#>), (<#>), (>-))
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.SocketTmux (testSocketTmux)

hi1 :: Map Text Text
hi1 =
  [("cterm", "reverse"), ("ctermfg", "1")]

hi2 :: Map Text Text
hi2 =
  [("cterm", "reverse"), ("ctermfg", "2")]

hi3 :: Map Text Text
hi3 =
  [("cterm", "reverse"), ("ctermfg", "3")]

syntax :: Syntax
syntax =
  build $ prefix "Test" do
    top1 <#> top2
  where
    top1 =
      region "Signature" [exon|\v^\w+ ::|] "$" #> hi hi1 (match "Colons" "::") >- link "Type" (match "Type" ".*")

    top2 = match "S" [exon|^S .\+$|] #> ((subA <#> subB) >- end)

    subA = hi hi1 (match "A" "A a") #> hi hi2 (match "AInner" "a")

    subB = hi hi1 (match "B" "B")

    end = hi hi3 (match "E" [exon|E \d|])

target :: Text
target =
  [exon|syntax match TestType /.*/ skipwhite contained
syntax match TestColons /::/ skipwhite contained nextgroup=TestType
syntax region TestSignature start=/\v^\w+ ::/ end=/$/ skipwhite contains=TestColons
syntax match TestE /E \d/ skipwhite contained
syntax match TestAInner /a/ skipwhite contained
syntax match TestA /A a/ skipwhite contained contains=TestAInner nextgroup=TestE
syntax match TestB /B/ skipwhite contained nextgroup=TestE
syntax match TestS /^S .\+$/ skipwhite contains=TestA,TestB
highlight default TestColons cterm=reverse ctermfg=1
highlight default TestE cterm=reverse ctermfg=3
highlight default TestAInner cterm=reverse ctermfg=2
highlight default TestA cterm=reverse ctermfg=1
highlight default TestB cterm=reverse ctermfg=1
highlight default link TestType Type
|]

test_syntax :: UnitTest
test_syntax =
  testSocketTmux do
    setCurrentBufferContent ["function :: String -> Int", "function _ = 5", "S A a E 1", "S B E 2"]
    Text.lines target === syntaxCmdlines syntax
    executeSyntax syntax
    awaitScreenshot False "syntax" 0
