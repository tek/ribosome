module Ribosome.Host.Interpreter.Errors where

import qualified Data.Map.Strict as Map
import Polysemy.Conc (interpretAtomic)

import Ribosome.Host.Data.HandlerError (ErrorMessage, HandlerTag)
import qualified Ribosome.Host.Effect.Errors as Errors
import Ribosome.Host.Effect.Errors (Errors)

interpretErrorsAtomic ::
  Member (AtomicState (Map HandlerTag [ErrorMessage])) r =>
  InterpreterFor Errors r
interpretErrorsAtomic =
  interpret \case
    Errors.Store htag msg ->
      atomicModify' (Map.alter alter (fromMaybe "global" htag))
      where
        alter =
          Just . maybe [msg] (msg :)
    Errors.Get ->
      atomicGet

interpretErrors ::
  Member (Embed IO) r =>
  InterpreterFor Errors r
interpretErrors =
  interpretAtomic mempty .
  interpretErrorsAtomic .
  raiseUnder
