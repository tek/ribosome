module Ribosome.Menu.Stream.Util where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, readTMChan)
import Data.Maybe (fromJust)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream, MonadAsync)
import Ribosome.Final (inFinal_)
import qualified Queue

takeUntilNothing ::
  Monad m =>
  IsStream t =>
  Functor (t m) =>
  t m (Maybe a) ->
  t m a
takeUntilNothing s =
  fromJust <$> Stream.takeWhile isJust s

chanStream ::
  IsStream t =>
  MonadAsync m =>
  Functor (t m) =>
  TMChan a ->
  t m a
chanStream chan =
  takeUntilNothing (Stream.repeatM (liftIO (atomically (readTMChan chan))))

queueStream ::
  IsStream t =>
  Functor (t IO) =>
  Members [Queue a, Final IO] r =>
  Sem r (t IO a)
queueStream =
  inFinal_ \ lowerMaybe _ pureF ->
    pureF do
      takeUntilNothing (Stream.repeatM (join <$> lowerMaybe Queue.readMaybe))
