{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ribosome.Prelude (
  module Control.Lens,
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
  mapLeft,
  tuple,
  undefined,
  unit,
  unsafeLogAnd,
  unsafeLogS,
  unsafeLogSAnd,
  throwText,
  (<$$>),
) where

import Control.Lens (makeClassy, (%~), (.~), (?~), (^.))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Resource.Internal (ResourceT (ResourceT))
import Cornea
import Data.Default (Default (def))
import Data.Either.Combinators (mapLeft)
import Data.Foldable (foldl, traverse_)
import Data.Functor.Syntax ((<$$>))
import GHC.Err (undefined)
import GHC.IO.Unsafe (unsafePerformIO)
import Relude hiding (Type, ask, asks, get, gets, hoistEither, hoistMaybe, local, modify, put, state, undefined)
import System.IO.Error (ioError, userError)

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

throwText ::
  MonadIO m =>
  Text ->
  m a
throwText =
  liftIO . ioError . userError . toString

instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
  type StM (ResourceT m) a = StM m a
  liftBaseWith f = ResourceT $ \reader' ->
      liftBaseWith $ \runInBase ->
          f $ runInBase . (\(ResourceT r) -> r reader'  )
  restoreM = ResourceT . const . restoreM
