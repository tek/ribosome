module Ribosome.Host.Data.StoredReport where

import qualified Chronos
import Polysemy.Chronos (ChronosTime)
import qualified Time

import Ribosome.Host.Data.Report (Report)

data StoredReport =
  StoredReport {
    report :: Report,
    time :: Chronos.Time
  }
  deriving stock (Show)

now ::
  Member ChronosTime r =>
  Report ->
  Sem r StoredReport
now r =
  StoredReport r <$> Time.now
