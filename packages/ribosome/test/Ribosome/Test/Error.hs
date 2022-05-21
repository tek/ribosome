module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

testError ::
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
testError =
  resumeHoistError (TestError . show)
