module Ribosome.Host.Effect.Reports where

import Ribosome.Host.Data.Report (Report, ReportContext)
import Ribosome.Host.Data.StoredReport (StoredReport)

data Reports :: Effect where
  StoreReport :: ReportContext -> Report -> Reports m ()
  StoredReports :: Reports m (Map ReportContext [StoredReport])

makeSem ''Reports
