module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (ToErrorMessage, mapHandlerError)
import Ribosome.Host.Data.RpcHandler (Handler)

resumeTestError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
resumeTestError =
  resumeHoistError (TestError . show)

testHandler ::
  Member (Error TestError) r =>
  Handler r a ->
  Sem r a
testHandler =
  stopToError . mapStop (TestError . HandlerError.user . HandlerError.msg) . raiseUnder

testError ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Error TestError) r =>
  Sem (Stop e : r) a ->
  Sem r a
testError =
  testHandler . mapHandlerError . raiseUnder
