module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.RpcHandler (Handler)

testError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
testError =
  resumeHoistError (TestError . show)

testHandler ::
  Member (Error TestError) r =>
  Handler r a ->
  Sem r a
testHandler =
  stopToError . mapStop (TestError . HandlerError.user . HandlerError.msg) . raiseUnder
