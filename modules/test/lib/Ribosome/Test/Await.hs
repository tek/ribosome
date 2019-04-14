module Ribosome.Test.Await where

import Control.Exception (throw)
import Control.Monad.Error.Class (MonadError, catchError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Test.Framework.AssertM (AssertM)

import Ribosome.Control.Concurrent.Wait (WaitError(Thrown), waitIODef)

await ::
  Show e =>
  MonadError e m =>
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  AssertM m =>
  (a -> m b) ->
  m a ->
  m b
await assertion acquire = do
  r <- waitIODef acquire' check'
  either failure return r
  where
    acquire' = catchError (Right <$> acquire) (return . Left . show)
    check' (Right a) = Right <$> assertion a
    check' (Left e) = return (Left e)
    failure (Thrown e) = throw e
    failure e = fail $ "await failed with " <> show e

await' ::
  ∀ a b m.
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  AssertM (ExceptT () m) =>
  (a -> m b) ->
  m a ->
  m b
await' assertion acquire = do
  r <- runExceptT $ await assertion' (lift acquire)
  either (const $ fail "internal error") return r
  where
    assertion' :: a -> ExceptT () m b
    assertion' = lift . assertion
