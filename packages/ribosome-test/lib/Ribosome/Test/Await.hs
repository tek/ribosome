module Ribosome.Test.Await where

import Hedgehog (TestT)
import Control.Exception (throw)
import Control.Monad.Error.Class (MonadError (throwError), catchError)
import Hedgehog.Internal.Property (mkTestT, runTestT, Failure, Journal)

import Ribosome.Control.Concurrent.Wait (WaitError(Thrown, ConditionUnmet, NotStarted), waitIODef)
import Hedgehog ((===))

await ::
  ∀ e a b m .
  MonadError e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  (a -> TestT m b) ->
  m a ->
  TestT m b
await assertion acquire = do
  lift (waitIODef acquire' check') >>= \case
    Right a -> pure a
    Left (ConditionUnmet (Left (err, journal))) ->
      mkTestT (pure (Left err, journal))
    Left (ConditionUnmet (Right e)) ->
      throwError e
    Left (Thrown e) ->
      throw e
    Left NotStarted -> fail "await was not started"
  where
    acquire' :: m (Either e a)
    acquire' =
      catchError (Right <$> acquire) (pure . Left)
    check' :: Either e a -> m (Either (Either (Failure, Journal) e) b)
    check' (Right a) = do
      (result, journal) <- runTestT (assertion a)
      pure (mapLeft (Left . (,journal)) result)
    check' (Left e) = do
      pure (Left (Right e))

awaitEqual ::
  ∀ e a b m .
  Eq b =>
  Show b =>
  MonadIO m =>
  MonadError e m =>
  MonadBaseControl IO m =>
  b ->
  (a -> b) ->
  m a ->
  TestT m ()
awaitEqual target f =
  await (\ a -> target === f a)
