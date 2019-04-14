module Ribosome.Watch where

import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans.Control (MonadBaseControl)

-- TODO
-- store canceling callback in RibosomeInternal
startWatcher ::
  MonadBaseControl IO m =>
  m ()
startWatcher =
  -- fork
  undefined
