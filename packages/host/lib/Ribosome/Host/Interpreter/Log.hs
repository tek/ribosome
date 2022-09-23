module Ribosome.Host.Interpreter.Log where

import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Log (
  Log (Log),
  LogMessage (LogMessage),
  Severity (Warn),
  dataLog,
  formatLogEntry,
  interceptDataLogConc,
  interpretLogDataLogConc,
  interpretLogNull,
  setLogLevel,
  )
import Path (Abs, File, Path, toFilePath)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Log (interpretLogStderrLevelConc)
import Polysemy.Log.Handle (interpretDataLogHandleWith)
import Polysemy.Log.Log (interpretDataLog)
import System.IO (Handle, IOMode (AppendMode), hClose, openFile)

import Ribosome.Host.Api.Data (nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import qualified Ribosome.Host.Data.HostConfig as HostConfig
import Ribosome.Host.Data.HostConfig (LogConfig (LogConfig))
import Ribosome.Host.Data.Report (
  LogReport (LogReport),
  Report (Report),
  ReportLog,
  prefixReportContext',
  report,
  reportMessages,
  severity,
  )
import Ribosome.Host.Effect.Log (FileLog, StderrLog, fileLog, stderrLog)
import qualified Ribosome.Host.Effect.Reports as Reports
import Ribosome.Host.Effect.Reports (Reports)
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

logLogReport ::
  Show e =>
  Members [Rpc !! e, Reports, UserError, Log] r =>
  Severity ->
  LogReport ->
  Sem r ()
logLogReport minSeverity (LogReport msg@(Report user log severity) echo store context) =
  withFrozenCallStack do
    Log.log severity (Text.intercalate "\n" (maybeToList (prefixReportContext' context) <> log))
    when store (Reports.storeReport context msg)
    when echo (echoError minSeverity user severity)

interpretReportLogRpc ::
  Show e =>
  Members [Reader LogConfig, Rpc !! e, Reports, UserError, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor ReportLog r
interpretReportLogRpc sem = do
  LogConfig {..} <- ask
  interpretDataLog (logLogReport logLevelEcho) ((if dataLogConc then interceptDataLogConc 64 else id) sem)

interpretReportLogLog ::
  Member Log r =>
  InterpreterFor ReportLog r
interpretReportLogLog =
  interpretDataLog \ LogReport {report} -> Log.log (severity report) (reportMessages report)

interpretLogRpc ::
  Members [Log, ReportLog] r =>
  InterpreterFor Log r
interpretLogRpc =
  interpret \case
    Log (LogMessage severity msg) -> do
      dataLog (LogReport (Report msg [msg] severity) True (severity >= Warn) mempty)

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
  interpretLogDataLogConc 64 .
  raiseUnder
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
