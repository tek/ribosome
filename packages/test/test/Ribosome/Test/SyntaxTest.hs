module Ribosome.Test.SyntaxTest where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Test.Tmux (TmuxTestConf(..))
import Hedgehog (TestT)
import TestError (TestError)

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Syntax (
  Syntax(Syntax),
  syntaxHighlight,
  syntaxMatch,
  )
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (defaultTestConfig)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.Tmux (tmuxTest')

data SyntaxTestError =
  Test TestError
  |
  Tmux TmuxError

deepPrisms ''SyntaxTestError

instance ReportError SyntaxTestError where
  errorReport _ =
    undefined

syntax :: Syntax
syntax =
  Syntax [syntaxMatch "TestColons" "::"] [syntaxHighlight "TestColons"
    [("cterm", "reverse"), ("ctermfg", "1"), ("gui", "reverse"), ("guifg", "#dc322f")]] []

syntaxTest :: TestT (Ribo () SyntaxTestError) ()
syntaxTest = do
  lift (setCurrentBufferContent ["function :: String -> Int", "function _ = 5"])
  _ <- lift (executeSyntax syntax)
  sleep 1
  awaitScreenshot "syntax" False 0

test_syntax :: UnitTest
test_syntax =
  tmuxTest' def { ttcWidth = 300, ttcHeight = 51, ttcGui = False } (defaultTestConfig "syntax") def syntaxTest
