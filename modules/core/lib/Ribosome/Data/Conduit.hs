module Ribosome.Data.Conduit where

import Conduit (ConduitT, runConduit, yield, (.|))
import Control.Concurrent.Lifted (fork, killThread)
import Control.Concurrent.STM.TBMChan (TBMChan, closeTBMChan, newTBMChan, readTBMChan, writeTBMChan)
import Control.Exception.Lifted (bracket, finally)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Conduit.Combinators as Conduit (mapM_)

withTBMChan ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Int ->
  (TBMChan a -> m b) ->
  m b
withTBMChan bound =
  bracket acquire release
  where
    acquire =
      atomically (newTBMChan bound)
    release =
      atomically . closeTBMChan

sourceChan ::
  MonadIO m =>
  TBMChan a ->
  ConduitT () a m ()
sourceChan chan =
  loop
  where
    loop =
      traverse_ recurse =<< atomically (readTBMChan chan)
    recurse a =
      yield a *> loop

withSourcesInChan ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ConduitT a Void m b ->
  [ConduitT () a m ()] ->
  TBMChan a ->
  m b
withSourcesInChan consumer sources chan = do
  threadIds <- traverse (fork . runConduit . start) sources
  finally listen (release threadIds)
  where
    release =
      traverse_ killThread
    listen =
      runConduit $ sourceChan chan .| consumer
    start source =
      source .| Conduit.mapM_ (atomically . writeTBMChan chan)

withMergedSources ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Int ->
  ConduitT a Void m b ->
  [ConduitT () a m ()] ->
  m b
withMergedSources bound consumer sources =
  withTBMChan bound (withSourcesInChan consumer sources)
