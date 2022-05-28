module Ribosome.Host.Interpreter.Errors where

import qualified Data.Map.Strict as Map
import Polysemy.Chronos (ChronosTime)
import Polysemy.Conc (interpretAtomic)

import Ribosome.Host.Data.HandlerError (HandlerTag)
import qualified Ribosome.Host.Data.StoredError as StoredError
import Ribosome.Host.Data.StoredError (StoredError)
import qualified Ribosome.Host.Effect.Errors as Errors
import Ribosome.Host.Effect.Errors (Errors)

interpretErrorsAtomic ::
  Members [AtomicState (Map HandlerTag [StoredError]), ChronosTime] r =>
  InterpreterFor Errors r
interpretErrorsAtomic =
  interpret \case
    Errors.Store htag msg -> do
      se <- StoredError.now msg
      atomicModify' (Map.alter (alter se) htag)
      where
        alter se =
          Just . maybe [se] (se :)
    Errors.Get ->
      atomicGet

interpretErrors ::
  Members [ChronosTime, Embed IO] r =>
  InterpreterFor Errors r
interpretErrors =
  interpretAtomic mempty .
  interpretErrorsAtomic .
  raiseUnder
