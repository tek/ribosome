-- |Data type that attaches a time stamp to a 'Report'.
module Ribosome.Host.Data.StoredReport where

import qualified Chronos
import Polysemy.Chronos (ChronosTime)
import qualified Time

import Ribosome.Host.Data.Report (Report)

-- |Data type that attaches a time stamp to a 'Report'.
data StoredReport =
  StoredReport {
    report :: Report,
    time :: Chronos.Time
  }
  deriving stock (Show)

-- |Create a new 'StoredReport' by querying the current time from 'ChronosTime'.
now ::
  Member ChronosTime r =>
  Report ->
  Sem r StoredReport
now r =
  StoredReport r <$> Time.now
