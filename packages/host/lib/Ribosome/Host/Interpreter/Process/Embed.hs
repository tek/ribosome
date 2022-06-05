module Ribosome.Host.Interpreter.Process.Embed where

import Data.Serialize (Serialize)
import Polysemy.Process (Process, ProcessOptions, withProcess)
import System.Process.Typed (ProcessConfig, proc)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Interpreter.Process.Cereal (interpretProcessCerealNative)

nvimArgs :: [String]
nvimArgs =
  ["--embed", "-n", "-u", "NONE", "-i", "NONE", "--clean", "--headless"]

nvimProc :: ProcessConfig () () ()
nvimProc =
  proc "nvim" nvimArgs

interpretProcessCerealNvimEmbed ::
  Serialize a =>
  Members [Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  Maybe ProcessOptions ->
  Maybe (ProcessConfig () () ()) ->
  InterpreterFor (Process a (Either Text a)) r
interpretProcessCerealNvimEmbed options conf =
  interpretProcessCerealNative (fromMaybe def options) (fromMaybe nvimProc conf) .
  resumeHoistError (BootError . show) .
  withProcess @() .
  raiseUnder2
