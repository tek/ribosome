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

withSourcesInChanAs ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (ConduitT () a m () -> m b) ->
  [ConduitT () a m ()] ->
  TBMChan a ->
  m b
withSourcesInChanAs executor sources chan = do
  threadIds <- traverse (fork . runConduit . start) sources
  finally listen (release threadIds)
  where
    release =
      traverse_ killThread
    listen =
      executor $ sourceChan chan
    start source =
      source .| Conduit.mapM_ (atomically . writeTBMChan chan)

simpleExecutor ::
  Monad m =>
  ConduitT a Void m b ->
  ConduitT () a m () ->
  m b
simpleExecutor consumer s =
  runConduit $ s .| consumer

withSourcesInChan ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ConduitT a Void m b ->
  [ConduitT () a m ()] ->
  TBMChan a ->
  m b
withSourcesInChan =
  withSourcesInChanAs . simpleExecutor

withMergedSourcesAs ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (ConduitT () a m () -> m b) ->
  Int ->
  [ConduitT () a m ()] ->
  m b
withMergedSourcesAs executor bound sources =
  withTBMChan bound (withSourcesInChanAs executor sources)

withMergedSources ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ConduitT a Void m b ->
  Int ->
  [ConduitT () a m ()] ->
  m b
withMergedSources =
  withMergedSourcesAs . simpleExecutor
