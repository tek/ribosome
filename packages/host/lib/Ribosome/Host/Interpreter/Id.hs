module Ribosome.Host.Interpreter.Id where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Input (Input (Input))

-- |Interpret 'Input' by incrementing a numeric type starting from @1@.
interpretInputNum ::
  ∀ a r .
  Num a =>
  Member (Embed IO) r =>
  InterpreterFor (Input a) r
interpretInputNum =
  interpretAtomic @a 1 .
  reinterpret \ Input ->
    atomicState' \ i -> (i + 1, i)
