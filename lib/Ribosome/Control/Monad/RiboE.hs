{-# LANGUAGE MultiParamTypeClasses #-}

module Ribosome.Control.Monad.RiboE(
  Ribo,
  RiboE(..),
  riboE,
  liftRibo,
  runRiboE,
  mapE,
  anaE,
  cataE,
  runRiboReport,
) where

import Control.Monad (join)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(ExceptT), mapExceptT, runExceptT)
import Data.Either.Combinators (mapLeft)
import Neovim.Context.Internal (Neovim)
import UnliftIO.STM (TVar)

import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (ReportError, reportError)

type Ribo e = Neovim (Ribosome (TVar e))

newtype RiboE s e a =
  RiboE { unRiboE :: ExceptT e (Ribo s) a }
  deriving (Functor, Applicative, Monad, MonadIO)

riboE :: (Ribo s) (Either e a) -> RiboE s e a
riboE = RiboE . ExceptT

liftRibo :: Ribo s a -> RiboE s e a
liftRibo = RiboE . lift

runRiboE :: RiboE s e a -> Ribo s (Either e a)
runRiboE = runExceptT . unRiboE

runRiboReport :: ReportError e => String -> RiboE s e () -> Ribo s ()
runRiboReport componentName ma = do
  result <- runRiboE ma
  case result of
    Right _ -> return ()
    Left e -> reportError componentName e

mapE :: (e -> e') -> RiboE s e a -> RiboE s e' a
mapE f =
  RiboE . trans . unRiboE
  where
    trans = mapExceptT (fmap $ mapLeft f)

anaE :: (e -> e') -> RiboE s e (Either e' a) -> RiboE s e' a
anaE f =
  riboE . fmap (join . mapLeft f) . runRiboE

cataE :: (e' -> e) -> RiboE s e (Either e' a) -> RiboE s e a
cataE f =
  riboE . fmap join . runRiboE . fmap (mapLeft f)

instance MonadError e (RiboE s e) where
  throwError =
    RiboE . throwError
  catchError ma f =
    RiboE $ catchError (unRiboE ma) (unRiboE . f)
