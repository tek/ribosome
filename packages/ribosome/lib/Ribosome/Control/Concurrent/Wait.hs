module Ribosome.Control.Concurrent.Wait where

import Control.Exception.Lifted (try)
import qualified Text.Show

import Ribosome.System.Time (sleep)

-- |Specifies the maximum number of retries and the interval in seconds for 'waitIO'.
data Retry =
  Retry Int Double
  deriving Show

instance Default Retry where
  def = Retry 30 0.1

-- |Error description for 'waitIO'
data WaitError e =
  NotStarted
  |
  ConditionUnmet e
  |
  ∀ excp. Exception excp => Thrown excp

instance Show e => Show (WaitError e) where
  show NotStarted =
    "NotStarted"
  show (ConditionUnmet reason) =
    "ConditionUnmet(" <> show reason <> ")"
  show (Thrown _) =
    "Thrown"

-- |Execute an IO thunk repeatedly until either the supplied condition produces a 'Right' or the maximum number of
-- retries specified in the `Retry` parameter has been reached.
-- Returns the value produced by the condition.
waitIO ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Retry ->
  m a ->
  (a -> m (Either e b)) ->
  m (Either (WaitError e) b)
waitIO (Retry maxRetry interval) thunk cond =
  wait maxRetry (Left NotStarted)
  where
    wait 0 reason = return reason
    wait count _ = do
      ea <- try thunk
      result <- try $ check ea
      case result of
        Right (Right a) ->
          return $ Right a
        Right (Left reason) ->
          recurse reason count
        Left (SomeException e) ->
          recurse (Thrown e) count
    recurse reason count = do
      sleep interval
      wait (count - 1) (Left reason)
    check (Right a) =
      cond a <&> \case
        Right b -> Right b
        Left reason -> Left (ConditionUnmet reason)
    check (Left (SomeException e)) =
      return $ Left (Thrown e)

-- |Calls 'waitIO' with the default configuration of 30 retries every 100ms.
waitIODef ::
  MonadIO m =>
  MonadBaseControl IO m =>
  m a ->
  (a -> m (Either e b)) ->
  m (Either (WaitError e) b)
waitIODef =
  waitIO def

-- |Same as 'waitIO', but the condition returns 'Bool' and the result is the result of the thunk.
waitIOPred ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Retry ->
  m a ->
  (a -> m Bool) ->
  m (Either (WaitError Text) a)
waitIOPred retry thunk pred' =
  waitIO retry thunk cond
  where
    cond a = pred' a <&> \case
      True -> Right a
      False -> Left ("predicate returned False" :: Text)

-- |Calls 'waitIOPred' with the default configuration of 30 retries every 100ms.
waitIOPredDef ::
  MonadIO m =>
  MonadBaseControl IO m =>
  m a ->
  (a -> m Bool) ->
  m (Either (WaitError Text) a)
waitIOPredDef =
  waitIOPred def
