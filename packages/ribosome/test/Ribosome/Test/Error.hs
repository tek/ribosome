-- |Combinators for aborting tests on Ribosome errors
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

-- |Convert the effect @eff@ to @eff '!!' err@ and @'Error' 'TestError'@, failing the test when 'Stop' is used.
resumeTestError ::
  ∀ eff e r .
  Show e =>
  Members [eff !! e, Error TestError] r =>
  InterpreterFor eff r
resumeTestError =
  resumeHoistError (TestError . show)

-- |Convert a 'HandlerError' from a 'Handler' to an @'Error' 'TestError'@, failing the test when 'Stop' is used.
testHandler ::
  Member (Error TestError) r =>
  Handler r a ->
  Sem r a
testHandler =
  stopToError . mapStop (TestError . message) . raiseUnder
  where
    message (HandlerError (ErrorMessage user log _) htag) =
      unlines ([exon|#{handlerTagName htag}:|] : user : log)

-- |Reinterpret @'Stop' err@ to @'Error' 'TestError'@ if @err@ is an instance of 'ToErrorMessage'.
testError ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Error TestError) r =>
  Sem (Stop e : r) a ->
  Sem r a
testError =
  testHandler . mapHandlerError . raiseUnder
