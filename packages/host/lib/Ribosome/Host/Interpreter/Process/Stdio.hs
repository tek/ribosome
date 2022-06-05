module Ribosome.Host.Interpreter.Process.Stdio where

import Data.Serialize (Serialize)
import Polysemy.Process (Process, interpretProcessCurrent)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Process.Cereal (interpretProcessInputCereal, interpretProcessOutputCereal)

interpretProcessCerealStdio ::
  Serialize a =>
  Members IOStack r =>
  InterpreterFor (Process a (Either Text a)) r
interpretProcessCerealStdio =
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcessCurrent def .
  raiseUnder2 .
  resumeHoistError (BootError . show @Text) .
  raiseUnder
