module Ribosome.Data.Conduit where

import Conduit (ConduitT, MonadResource, bracketP, runConduit, yield, (.|))
import Control.Concurrent (forkIO)
import Control.Concurrent.Lifted (fork, killThread)
import Control.Concurrent.STM.TBMChan (TBMChan, closeTBMChan, newTBMChan, readTBMChan, writeTBMChan)
import Control.Exception.Lifted (bracket, finally)
import Control.Monad.Trans.Control (embed)
import qualified Data.Conduit.Combinators as Conduit (mapM_)

import Ribosome.Control.Monad.Ribo (modifyTMVar)

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

sourceTerminated ::
  MonadIO m =>
  MonadBaseControl IO m =>
  TMVar Int ->
  TBMChan a ->
  m ()
sourceTerminated var chan = do
  n <- modifyTMVar (subtract 1) var
  when (n == 0) (atomically $ closeTBMChan chan)

mergeSourcesWith ::
  MonadResource m =>
  MonadBaseControl IO m =>
  TMVar Int ->
  TBMChan a ->
  (ConduitT () a m () -> IO (StM m ())) ->
  [ConduitT () a m ()] ->
  ConduitT () a m ()
mergeSourcesWith activeSources chan sourceRunner sources =
  bracketP acquire release (const combinedSource)
  where
    acquire =
      traverse (forkIO . start) sources
    start source = do
      void $ sourceRunner source
      sourceTerminated activeSources chan
    release ids =
      traverse_ killThread ids *>
      atomically (closeTBMChan chan)
    combinedSource =
      sourceChan chan

mergeSources ::
  MonadResource m =>
  MonadBaseControl IO m =>
  Int ->
  [ConduitT () a m ()] ->
  ConduitT () a m ()
mergeSources bound sources = do
  activeSources <- atomically $ newTMVar (length sources)
  chan <- atomically (newTBMChan bound)
  embeddedRunner <- lift $ embed (embedSourceRunner chan)
  mergeSourcesWith activeSources chan embeddedRunner sources
  where
    embedSourceRunner chan source =
      runConduit (source .| Conduit.mapM_ (atomically . writeTBMChan chan))

withSourcesInChanAs ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (ConduitT () a m () -> m b) ->
  [ConduitT () a m ()] ->
  TBMChan a ->
  m b
withSourcesInChanAs executor sources chan = do
  activeSources <- atomically $ newTMVar (length sources)
  threadIds <- traverse (fork . start activeSources) sources
  finally listen (release threadIds)
  where
    release =
      traverse_ killThread
    listen =
      executor $ sourceChan chan
    start activeSources source = do
      runConduit (source .| Conduit.mapM_ (atomically . writeTBMChan chan))
      sourceTerminated activeSources chan

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
