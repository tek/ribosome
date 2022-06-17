module Ribosome.Host.Interpreter.Log where

import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Log (
  Log (Log),
  Severity,
  formatLogEntry,
  interceptDataLogConc,
  interpretLogDataLogConc,
  interpretLogNull,
  setLogLevel,
  )
import Path (Abs, File, Path, toFilePath)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Log (interpretLogStderrLevelConc)
import Polysemy.Log.Handle (interpretDataLogHandleWith)
import Polysemy.Log.Log (interpretDataLog)
import System.IO (Handle, IOMode (AppendMode), hClose, openFile)

import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), HandlerError (HandlerError))
import qualified Ribosome.Host.Data.HostConfig as HostConfig
import Ribosome.Host.Data.HostConfig (LogConfig (LogConfig))
import Ribosome.Host.Data.HostError (HostError (HostError))
import qualified Ribosome.Host.Effect.Errors as Errors
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Log (FileLog, StderrLog, fileLog, stderrLog)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError, userError)

echoError ::
  Show e =>
  Members [Rpc !! e, UserError, Log] r =>
  Severity ->
  Text ->
  Severity ->
  Sem r ()
echoError minSeverity err severity | severity >= minSeverity =
  userError err severity >>= traverse_ \ msg ->
    nvimEcho [toMsgpack @[_] msg] True mempty !! \ e' ->
      Log.error [exon|Couldn't echo handler error: #{show e'}|]
echoError _ _ _ =
  unit

logHandlerError ::
  Show e =>
  Members [Rpc !! e, Errors, UserError, Log] r =>
  Severity ->
  HostError ->
  Sem r ()
logHandlerError minSeverity (HostError report (HandlerError msg@(ErrorMessage user log severity) htag)) = do
  Log.log severity (Text.unlines log)
  Errors.store htag msg
  when report (echoError minSeverity user severity)

interpretDataLogRpc ::
  Show e =>
  Members [Reader LogConfig, Rpc !! e, Errors, UserError, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (DataLog HostError) r
interpretDataLogRpc sem = do
  LogConfig {..} <- ask
  interpretDataLog (logHandlerError logLevelEcho) ((if dataLogConc then interceptDataLogConc 64 else id) sem)

interpretLogStderrFile ::
  Members [StderrLog, FileLog] r =>
  InterpreterFor Log r
interpretLogStderrFile =
  interpret \case
    Log m ->
      fileLog (send (Log m)) *> stderrLog (send (Log m))

interpretLogHandleLevel ::
  Members [Resource, ChronosTime, Race, Async, Embed IO] r =>
  Handle ->
  Maybe Severity ->
  InterpreterFor Log r
interpretLogHandleLevel handle level =
  interpretDataLogHandleWith handle formatLogEntry .
  setLogLevel level .
  interpretTimeChronos .
  interpretLogDataLogConc 64 .
  raiseUnder2
{-# inline interpretLogHandleLevel #-}

interpretLogFileLevel ::
  Members [Resource, ChronosTime, Race, Async, Embed IO] r =>
  Maybe Severity ->
  Path Abs File ->
  InterpreterFor Log r
interpretLogFileLevel level path sem =
  bracket acquire (embed . hClose) \ handle ->
    interpretLogHandleLevel handle level sem
  where
    acquire =
      embed (openFile (toFilePath path) AppendMode)
{-# inline interpretLogFileLevel #-}

interpretLogs ::
  Members [Reader LogConfig, Resource, ChronosTime, Race, Async, Embed IO] r =>
  InterpretersFor [StderrLog, FileLog] r
interpretLogs sem =
  ask >>= \ LogConfig {..} ->
    maybe interpretLogNull (\ f -> interpretLogFileLevel (Just logLevelFile) f) logFile $
    untag @"file" $
    interpretLogStderrLevelConc (Just logLevelStderr) $
    untag @"stderr" sem
