{-# LANGUAGE NoImplicitPrelude #-}

module Ribosome.Prelude (
  module Control.Monad.DeepError,
  module Control.Monad.DeepState,
  module Control.Monad.Trans.Control,
  module Data.DeepLenses,
  module Data.DeepPrisms,
  module Data.Default,
  module Data.Foldable,
  module Relude,
  dbg,
  dbgs,
  dbgm,
  mapLeft,
  tuple,
  undefined,
  unit,
  unsafeLog,
  unsafeLogS,
  (<$$>),
) where

import Control.Monad.DeepError (MonadDeepError(throwHoist), catchAt, hoistEither)
import Control.Monad.DeepState (
  MonadDeepState,
  get,
  getL,
  gets,
  getsL,
  modify,
  modifyL,
  modifyM,
  modifyM',
  put,
  setL,
  stateM,
  )
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.DeepLenses (deepLenses)
import Data.DeepPrisms (deepPrisms)
import Data.Default (Default(def))
import Data.Either.Combinators (mapLeft)
import Data.Foldable (foldl, traverse_)
import Data.Functor (void)
import Data.Functor.Syntax ((<$$>))
import GHC.Err (undefined)
import GHC.IO.Unsafe (unsafePerformIO)
import Relude hiding (undefined, Type, state, modify, gets, get, put)

dbg :: Monad m => Text -> m ()
dbg msg = do
  () <- return $ unsafePerformIO (putStrLn msg)
  return ()

dbgs :: Monad m => Show a => a -> m ()
dbgs =
  dbg . show

dbgm :: Monad m => Show a => m a -> m a
dbgm ma = do
  a <- ma
  dbgs a
  return a

unit ::
  Applicative f =>
  f ()
unit =
  pure ()

tuple ::
  Applicative f =>
  f a ->
  f b ->
  f (a, b)
tuple fa fb =
  (,) <$> fa <*> fb

unsafeLogS :: Show a => a -> b -> b
unsafeLogS a b = unsafePerformIO $ print a >> return b

unsafeLog :: Text -> b -> b
unsafeLog a b = unsafePerformIO $ putStrLn a >> return b
