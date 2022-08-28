-- |Interpreters for 'Reports'.
module Ribosome.Host.Interpreter.Reports where

import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Polysemy.Chronos (ChronosTime)

import Ribosome.Host.Data.Report (ReportContext)
import qualified Ribosome.Host.Data.StoredReport as StoredReport
import Ribosome.Host.Data.StoredReport (StoredReport)
import qualified Ribosome.Host.Effect.Reports as Reports
import Ribosome.Host.Effect.Reports (Reports)

-- |Interpret 'Reports' by storing reports in 'AtomicState'.
interpretReportsAtomic ::
  Members [AtomicState (Map ReportContext [StoredReport]), ChronosTime] r =>
  Int ->
  InterpreterFor Reports r
interpretReportsAtomic maxReports =
  interpret \case
    Reports.StoreReport htag msg -> do
      sr <- StoredReport.now msg
      atomicModify' (Map.alter (alter sr) htag)
      where
        alter sr =
          Just . take maxReports . maybe [sr] (sr :)
    Reports.StoredReports ->
      atomicGet

-- |Interpret 'Reports' by storing reports in 'AtomicState' and interpret the state effect.
interpretReports ::
  Members [ChronosTime, Embed IO] r =>
  InterpreterFor Reports r
interpretReports =
  interpretAtomic mempty .
  interpretReportsAtomic 100 .
  raiseUnder
