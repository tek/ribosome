-- |Smart constructors for @'DataLog' 'LogReport'@
module Ribosome.Report (
  module Ribosome.Report,
  module Ribosome.Host.Effect.Reports,
  module Ribosome.Host.Interpreter.Reports,
  LogReport (LogReport),
  Report (Report),
  ReportContext (ReportContext),
  Reportable (toReport),
) where

import Log (Severity (Error, Info, Warn), dataLog)
import qualified Polysemy.Log as Log

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import qualified Ribosome.Host.Data.Report as Report
import Ribosome.Host.Data.Report (
  LogReport (LogReport),
  Report (Report),
  ReportContext (ReportContext),
  Reportable (toReport),
  resumeReport,
  severity,
  )
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Reports (Reports (..), storeReport, storedReports)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Reports

-- |Add a segment to the current 'Report' logging context.
context ::
  Member (DataLog LogReport) r =>
  Text ->
  Sem r a ->
  Sem r a
context ctx =
  Log.local (#context %~ \ (ReportContext cur) -> ReportContext (ctx : cur))

-- |Set the current 'Report' logging context.
setContext ::
  Member (DataLog LogReport) r =>
  ReportContext ->
  Sem r a ->
  Sem r a
setContext ctx =
  Log.local (#context .~ ctx)

-- |Convert a value to 'Report' via 'Reportable' and send it to the log.
logReport ::
  Reportable e =>
  Member (DataLog LogReport) r =>
  e ->
  Sem r ()
logReport e =
  dataLog (LogReport r True (severity r >= Warn) mempty)
  where
    r = toReport e

-- |Send a 'Report' to the log given a user and log message, with serverity 'Info'.
info ::
  Member (DataLog LogReport) r =>
  Text ->
  [Text] ->
  Sem r ()
info user log =
  logReport Report {severity = Info, ..}

-- |Send a 'Report' to the log given a user and log message, with serverity 'Warn'.
warn ::
  Member (DataLog LogReport) r =>
  Text ->
  [Text] ->
  Sem r ()
warn user log =
  logReport Report {severity = Warn, ..}

-- |Send a 'Report' to the log given a user and log message, with serverity 'Error'.
error ::
  Member (DataLog LogReport) r =>
  Text ->
  [Text] ->
  Sem r ()
error user log =
  logReport Report {severity = Error, ..}

-- |Eliminate @'Stop' err@ by converting @err@ to a 'Report' and logging it, continuing execution for a unit action.
reportStop ::
  ∀ err r .
  Reportable err =>
  Member (DataLog LogReport) r =>
  Sem (Stop err : r) () ->
  Sem r ()
reportStop sem =
  withFrozenCallStack do
    either logReport pure =<< runStop sem

-- |Resume an effect by converting the error to a 'Report' and logging it, continuing execution for a unit action.
resumeLogReport ::
  ∀ eff e r .
  Reportable e =>
  Members [eff !! e, DataLog LogReport] r =>
  Sem (eff : r) () ->
  Sem r ()
resumeLogReport sem =
  withFrozenCallStack do
    sem !! logReport

-- |Resume all plugin effects.
pluginLogReports ::
  Members [Scratch !! RpcError, Settings !! SettingError, Rpc !! RpcError, Stop Report] r =>
  InterpretersFor [Scratch, Settings, Rpc] r
pluginLogReports =
  resumeReport @Rpc .
  resumeReport @Settings .
  resumeReport @Scratch
