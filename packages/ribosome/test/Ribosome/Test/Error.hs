-- |Combinators for aborting tests on Ribosome errors
module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

import Ribosome.Host.Data.Report (Reportable, mapReport, reportMessages)
import Ribosome.Host.Data.RpcHandler (Handler)

-- |Convert the effect @eff@ to @eff '!!' err@ and @'Error' 'TestError'@, failing the test when 'Stop' is used.
resumeTestError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
resumeTestError =
  resumeHoistError (TestError . show)

-- |Convert a 'LogReport' from a 'Handler' to an @'Error' 'TestError'@, failing the test when 'Stop' is used.
testHandler ::
  Member (Error TestError) r =>
  Handler r a ->
  Sem r a
testHandler =
  stopToError . mapStop (TestError . reportMessages) . raiseUnder

-- |Reinterpret @'Stop' err@ to @'Error' 'TestError'@ if @err@ is an instance of 'Reportable'.
testError ::
  ∀ e r a .
  Reportable e =>
  Member (Error TestError) r =>
  Sem (Stop e : r) a ->
  Sem r a
testError =
  testHandler . mapReport . raiseUnder
