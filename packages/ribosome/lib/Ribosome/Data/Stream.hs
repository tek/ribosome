module Ribosome.Data.Stream where

import Control.Concurrent.STM.TMChan (TMChan, readTMChan)
import Control.Monad.Catch (MonadThrow)
import Relude.Unsafe (fromJust)
import qualified Streamly.Prelude as Streamly
import Streamly.Prelude (IsStream)

takeUntilNothing ::
  Monad m =>
  IsStream t =>
  t m (Maybe a) ->
  t m a
takeUntilNothing s =
  fromJust <$> Streamly.takeWhile isJust s

chanStream ::
  MonadIO m =>
  IsStream t =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  TMChan a ->
  t m a
chanStream chan =
  takeUntilNothing (Streamly.repeatM (atomically (readTMChan chan)))
