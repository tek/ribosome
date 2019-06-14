module Ribosome.System.Time where

import Control.Concurrent (threadDelay)
import Data.Hourglass (Elapsed(Elapsed), ElapsedP(ElapsedP), NanoSeconds(NanoSeconds), Seconds(Seconds))
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Float (word2Double)

epochSeconds :: MonadIO m => m Int
epochSeconds = liftIO $ round <$> getPOSIXTime

usleep :: MonadIO m => Double -> m ()
usleep =
  liftIO . threadDelay . round

sleep :: MonadIO m => Double -> m ()
sleep seconds =
  usleep $ seconds * 1e6

sleepW :: MonadIO m => Word -> m ()
sleepW = sleep . word2Double

secondsP :: Double -> ElapsedP
secondsP s =
  ElapsedP (Elapsed (Seconds $ floor s)) (NanoSeconds nano)
  where
    nano =
      round (s * 1000000000) `mod` 1000000000
