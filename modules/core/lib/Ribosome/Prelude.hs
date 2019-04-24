{-# LANGUAGE NoImplicitPrelude #-}

module Ribosome.Prelude (
  module Control.Monad.DeepError,
  module Control.Monad.DeepState,
  module Data.DeepLenses,
  module Data.DeepPrisms,
  module Data.Foldable,
  module Relude,
  dbg,
  dbgs,
  mapLeft,
  undefined,
  (<$$>),
) where

import Control.Monad.DeepError (MonadDeepError(throwHoist), catchAt, hoistEither)
import Control.Monad.DeepState (MonadDeepState, get, getL, gets, getsL, modify, modifyL, put, setL)
import Data.DeepLenses (deepLenses)
import Data.DeepPrisms (deepPrisms)
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