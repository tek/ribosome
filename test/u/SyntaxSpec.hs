{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SyntaxSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.TmuxError (TmuxError)
import Control.Monad.IO.Class (liftIO)
import Data.DeepPrisms (deepPrisms)
import Test.Framework

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Control.Monad.Ribo (RiboN)
import Ribosome.Data.Syntax (Highlight(Highlight), Syntax(Syntax), SyntaxItem(Keyword), syntaxHighlight, syntaxMatch)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.IO (vimCommand, vimCommandOutput)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (defaultTestConfig)
import Ribosome.Test.Screenshot (assertScreenshot)
import Ribosome.Test.Tmux (tmuxGuiSpec)

data SyntaxSpecError =
  Rpc RpcError
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
  executeSyntax syntax
  sleep 0.5
  assertScreenshot "syntax" False 0

test_syntax :: IO ()
test_syntax =
  tmuxGuiSpec (defaultTestConfig "syntax") syntaxSpec
