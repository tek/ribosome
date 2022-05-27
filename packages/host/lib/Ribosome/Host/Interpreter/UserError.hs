module Ribosome.Host.Interpreter.UserError where

import Polysemy.Log (Severity (Info))

import Ribosome.Host.Effect.UserError (UserError (UserError))

interpretUserErrorInfo :: InterpreterFor UserError r
interpretUserErrorInfo =
  interpret \case
    UserError e severity | severity >= Info ->
      pure (Just [e])
    UserError _ _ ->
      pure Nothing
