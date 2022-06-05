module Ribosome.Host.IOStack where

import Conc (ConcStack)
import qualified Data.Text.IO as Text
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import System.IO (stderr)

import Ribosome.Host.Config (interpretLogConfig)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.HostConfig (HostConfig, LogConfig)
import Ribosome.Host.Effect.Log (FileLog, StderrLog)
import Ribosome.Host.Interpreter.Log (interpretLogStderrFile, interpretLogs)

type LogConfStack =
  [
    Log,
    StderrLog,
    FileLog,
    Reader LogConfig,
    Reader HostConfig
  ]

interpretLogConfStack ::
  Members [ChronosTime, Error BootError, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  InterpretersFor LogConfStack r
interpretLogConfStack conf =
  runReader conf .
  interpretLogConfig .
  interpretLogs .
  interpretLogStderrFile

type IOStack =
  [
    ChronosTime,
    Error BootError
  ] ++ ConcStack

errorStderr :: IO (Either BootError ()) -> IO ()
errorStderr ma =
  ma >>= \case
    Left (BootError err) -> Text.hPutStrLn stderr err
    Right () -> unit

runIOStack ::
  Sem IOStack () ->
  IO ()
runIOStack =
  errorStderr .
  runConc .
  errorToIOFinal .
  interpretTimeChronos

type BasicStack =
  LogConfStack ++ IOStack

runBasicStack ::
  HostConfig ->
  Sem BasicStack () ->
  IO ()
runBasicStack conf =
  runIOStack .
  interpretLogConfStack conf
