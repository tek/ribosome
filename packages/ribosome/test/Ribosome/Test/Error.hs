module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

testError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
testError =
  resumeHoistError (TestError . show)
