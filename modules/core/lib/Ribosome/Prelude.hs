{-# LANGUAGE NoImplicitPrelude #-}

module Ribosome.Prelude (
  module Control.Monad.Trans.Control,
  module Cornea,
  module Data.Default,
  module Data.Foldable,
  module Relude,
  dbg,
  dbgs,
  dbgm,
  dbgWith,
  dbgmWith,
  makeClassy,
  mapLeft,
  tuple,
  undefined,
  unit,
  unsafeLogAnd,
  unsafeLogS,
  unsafeLogSAnd,
  (<$$>),
) where

import Control.Lens (makeClassy)
import Control.Monad.Trans.Control (MonadBaseControl)
import Cornea
import Data.DeepLenses (deepLenses)
import Data.DeepPrisms (deepPrisms)
import Data.Default (Default(def))
import Data.Either.Combinators (mapLeft)
import Data.Foldable (foldl, traverse_)
import Data.Functor (void)
import Data.Functor.Syntax ((<$$>))
import GHC.Err (undefined)
import GHC.IO.Unsafe (unsafePerformIO)
import Relude hiding (undefined, Type, state, modify, gets, get, put, hoistMaybe, hoistEither)

dbg :: Monad m => Text -> m ()
dbg msg = do
  () <- return $ unsafePerformIO (putStrLn (toString msg))
  return ()

dbgs :: Monad m => Show a => a -> m ()
dbgs =
  dbg . show

dbgm :: Monad m => Show a => m a -> m a
dbgm ma = do
  a <- ma
  a <$ dbgs a

dbgWith ::
  Monad m =>
  Show b =>
  (a -> b) ->
  a ->
  m a
dbgWith f a =
  a <$ dbgs (f a)

dbgmWith ::
  Monad m =>
  Show b =>
  (a -> b) ->
  m a ->
  m a
dbgmWith f ma = do
  a <- ma
  a <$ dbgs (f a)

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

unsafeLogSAnd :: Show a => a -> b -> b
unsafeLogSAnd a b =
  unsafePerformIO $ print a >> return b

unsafeLogAnd :: Text -> b -> b
unsafeLogAnd a b =
  unsafePerformIO $ putStrLn (toString a) >> return b

unsafeLogS :: Show a => a -> a
unsafeLogS a =
  unsafePerformIO $ print a >> return a
