module Ribosome.Host.Interpreter.Log where

import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Log (Log (Log), Severity, formatLogEntry, interceptDataLogConc, interpretLogDataLogConc, interpretLogNull, setLogLevel)
import Path (Abs, File, Path, toFilePath)
import Polysemy.Log (interpretLogStderrLevelConc)
import Polysemy.Log.Handle (interpretDataLogHandleWith)
import Polysemy.Log.Log (interpretDataLog)
import System.IO (Handle, IOMode (AppendMode), hClose, openFile)
import Time (GhcTime, interpretTimeGhc)

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
  Text ->
  Severity ->
  Sem r ()
echoError err severity =
  userError err severity >>= traverse_ \ msg ->
    nvimEcho [toMsgpack @[_] msg] True mempty !! \ e' ->
      Log.error [exon|Couldn't echo handler error: #{show e'}|]

logHandlerError ::
  Show e =>
  Members [Rpc !! e, Errors, UserError, Log] r =>
  HostError ->
  Sem r ()
logHandlerError (HostError notification (HandlerError msg@(ErrorMessage user log severity) htag)) = do
  Log.log severity logLines
  Errors.store htag msg
  when notification (echoError user severity)
  where
    logLines =
      Text.unlines log

interpretDataLogRpc ::
  Show e =>
  Members [Rpc !! e, Errors, UserError, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (DataLog HostError) r
interpretDataLogRpc =
  interpretDataLog logHandlerError .
  interceptDataLogConc 64

interpretLogStderrFile ::
  Members [StderrLog, FileLog] r =>
  InterpreterFor Log r
interpretLogStderrFile =
  interpret \case
    Log m ->
      fileLog (send (Log m)) *> stderrLog (send (Log m))

interpretLogHandleLevel ::
  Members [Resource, GhcTime, Race, Async, Embed IO] r =>
  Handle ->
  Maybe Severity ->
  InterpreterFor Log r
interpretLogHandleLevel handle level =
  interpretDataLogHandleWith handle formatLogEntry .
  setLogLevel level .
  interpretTimeGhc .
  interpretLogDataLogConc 64 .
  raiseUnder2
{-# inline interpretLogHandleLevel #-}

interpretLogFileLevel ::
  Members [Resource, GhcTime, Race, Async, Embed IO] r =>
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
  Members [Reader LogConfig, Resource, GhcTime, Race, Async, Embed IO] r =>
  InterpretersFor [StderrLog, FileLog] r
interpretLogs sem =
  ask >>= \ LogConfig {..} ->
    maybe interpretLogNull (\ f -> interpretLogFileLevel (Just logLevelFile) f) logFile $
    untag @"file" $
    interpretLogStderrLevelConc (Just logLevelStderr) $
    untag @"stderr" sem
