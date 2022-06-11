module Ribosome.Test.Wait where

import Hedgehog.Internal.Property (Failure, failWith, liftTest, mkTest)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (interpretAtomic)
import Polysemy.Test (Hedgehog, liftH)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds))

assertWaitFor ::
  Monad m =>
  HasCallStack =>
  Members [Hedgehog m, Time t d, Race, Error Failure, Embed IO] r =>
  TimeUnit t1 =>
  TimeUnit t2 =>
  t1 ->
  t2 ->
  Sem r a ->
  (a -> Sem r b) ->
  Sem r b
assertWaitFor timeout interval acquire test =
  withFrozenCallStack do
    interpretAtomic Nothing do
      Conc.timeout_ timeoutError timeout spin
  where
    spin = do
      a <- raise acquire
      catch (raise (test a)) \ e -> do
        atomicPut (Just e)
        Time.sleep interval
        spin
    timeoutError =
      atomicGet >>= liftH . \case
        Just e -> liftTest (mkTest (Left e, mempty))
        Nothing -> failWith Nothing "timed out before an assertion was made"

assertWait ::
  Monad m =>
  HasCallStack =>
  Members [Hedgehog m, Time t d, Race, Error Failure, Embed IO] r =>
  Sem r a ->
  (a -> Sem r b) ->
  Sem r b
assertWait acquire test =
  withFrozenCallStack do
    assertWaitFor (Seconds 3) (MilliSeconds 100) acquire test
