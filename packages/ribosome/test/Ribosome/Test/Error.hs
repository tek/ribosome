module Ribosome.Test.Error where

import Exon (exon)
import Polysemy.Test (TestError (TestError))

import Ribosome.Host.Data.HandlerError (
  ErrorMessage (ErrorMessage),
  HandlerError (HandlerError),
  ToErrorMessage,
  handlerTagName,
  mapHandlerError,
  )
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
  stopToError . mapStop (TestError . message) . raiseUnder
  where
    message (HandlerError (ErrorMessage user log _) htag) =
      unlines ([exon|#{handlerTagName htag}:|] : user : log)

testError ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Error TestError) r =>
  Sem (Stop e : r) a ->
  Sem r a
testError =
  testHandler . mapHandlerError . raiseUnder
