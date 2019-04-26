{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SyntaxSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Test.Tmux (TmuxTestConf(..))
import Data.DeepPrisms (deepPrisms)
import Data.Default (def)
import Test.Framework

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Control.Monad.Ribo (RiboN)
import Ribosome.Data.Syntax (
  Syntax(Syntax),
  syntaxHighlight,
  syntaxMatch,
  )
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (defaultTestConfig)
import Ribosome.Test.Screenshot (assertScreenshot)
import Ribosome.Test.Tmux (tmuxSpec')
import TestError (TestError)

data SyntaxSpecError =
  Test TestError
  |
  Tmux TmuxError

deepPrisms ''SyntaxSpecError

instance ReportError SyntaxSpecError where
  errorReport _ =
    undefined

syntax :: Syntax
syntax =
  Syntax [syntaxMatch "TestColons" "::"] [syntaxHighlight "TestColons"
    [("cterm", "reverse"), ("ctermfg", "1"), ("gui", "reverse"), ("guifg", "#dc322f")]] []

syntaxSpec :: RiboN () SyntaxSpecError ()
syntaxSpec = do
  setCurrentBufferContent ["function :: String -> Int", "function _ = 5"]
  _ <- executeSyntax syntax
  sleep 0.5
  assertScreenshot "syntax" False 0

test_syntax :: IO ()
test_syntax =
  tmuxSpec' def { ttcWidth = 300, ttcHeight = 51, ttcGui = False } (defaultTestConfig "syntax") def syntaxSpec
