{-# options_haddock not-home #-}

module Ribosome.Test.Error where

import Polysemy.Test (TestError (TestError))

import Ribosome.Host.Data.Report (Reportable, mapReport, reportMessages)
import Ribosome.Host.Data.RpcHandler (Handler)

-- |Resume an effect and convert its error from @'Stop' err@ to @'Error' 'TestError'@.
resumeTestError ::
  HasCallStack =>
  ∀ eff err r .
  Show err =>
  Members [eff !! err, Error TestError] r =>
  InterpreterFor eff r
resumeTestError =
  withFrozenCallStack do
    resumeHoistError (TestError . show)

-- |Run a 'Handler', converting the @'Stop' 'Report'@ at its head to @'Error' 'TestError'@.
testHandler ::
  HasCallStack =>
  Member (Error TestError) r =>
  Handler r a ->
  Sem r a
testHandler =
  withFrozenCallStack do
    stopToErrorWith (TestError . reportMessages)

-- |Run a 'Handler' in a new thread and return an action that waits for the thread to terminate when sequenced.
-- Converts the @'Stop' 'Report'@ at its head to @'Error' 'TestError'@ when it is awaited.
testHandlerAsync ::
  HasCallStack =>
  Members [Error TestError, Async] r =>
  Handler r a ->
  Sem r (Sem r a)
testHandlerAsync h =
  withFrozenCallStack do
    thread <- async do
      runStop h
    pure do
      testHandler . stopEither =<< note (TestError "async handler didn't produce result") =<< await thread

-- |Interpret @'Stop' err@ to @'Error' 'TestError'@ by using @err@'s instance of 'Reportable'.
testError ::
  ∀ err r .
  HasCallStack =>
  Reportable err =>
  Member (Error TestError) r =>
  InterpreterFor (Stop err) r
testError ma =
  withFrozenCallStack do
    testHandler (mapReport (raiseUnder ma))
