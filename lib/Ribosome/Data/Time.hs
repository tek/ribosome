module Ribosome.Data.Time(
  epochSeconds,
  usleep,
  sleep,
  sleepW,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Float (word2Double)

epochSeconds :: MonadIO m => m Int
epochSeconds = liftIO $ fmap round getPOSIXTime

usleep :: MonadIO m => Double -> m ()
usleep =
  liftIO . threadDelay . round

sleep :: MonadIO m => Double -> m ()
sleep seconds =
  usleep $ seconds * 1e6

sleepW :: MonadIO m => Word -> m ()
sleepW = sleep . word2Double
