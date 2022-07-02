module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

import Ribosome.Host.Data.HandlerError (ToErrorMessage, handlerErrorMessage, mapHandlerError)
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
  stopToErrorWith (TestError . handlerErrorMessage)

testHandlerAsync ::
  Members [Error TestError, Async] r =>
  Handler r a ->
  Sem r (Sem r a)
testHandlerAsync h = do
  thread <- async do
    runStop h
  pure do
    testHandler . stopEither =<< note (TestError "async handler didn't produce result") =<< await thread

testError ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Error TestError) r =>
  Sem (Stop e : r) a ->
  Sem r a
testError =
  testHandler . mapHandlerError . raiseUnder
