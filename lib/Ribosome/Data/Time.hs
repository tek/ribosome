module Ribosome.Data.Time(
  epochSeconds,
  sleep,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

epochSeconds :: MonadIO f => f Int
epochSeconds = liftIO $ fmap round getPOSIXTime

sleep :: MonadIO f => Double -> f ()
sleep seconds =
  liftIO $ threadDelay $ round $ seconds * 1e6
