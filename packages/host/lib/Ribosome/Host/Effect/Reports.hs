-- |An effect for storing errors.
module Ribosome.Host.Effect.Reports where

import Ribosome.Host.Data.Report (Report, ReportContext)
import Ribosome.Host.Data.StoredReport (StoredReport)

-- |This internal effect stores all errors in memory that have been created through the 'Report' system.
data Reports :: Effect where
  -- |Add a report to the store.
  StoreReport :: ReportContext -> Report -> Reports m ()
  -- |Get all reports.
  StoredReports :: Reports m (Map ReportContext [StoredReport])

makeSem_ ''Reports

-- |Add a report to the store.
storeReport ::
  Member Reports r =>
  ReportContext ->
  Report ->
  Sem r ()

-- |Get all reports.
storedReports ::
  Member Reports r =>
  Sem r (Map ReportContext [StoredReport])
