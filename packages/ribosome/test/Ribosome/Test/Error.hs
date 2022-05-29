module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

resumeTestError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
resumeTestError =
  resumeHoistError (TestError . show)
