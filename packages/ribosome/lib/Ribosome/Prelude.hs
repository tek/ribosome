{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ribosome.Prelude (
  module Control.Lens,
  module Control.Monad.Trans.Control,
  module Cornea,
  module Exon,
  module Data.Default,
  module Data.Foldable,
  module Relude,
  module Ribosome.Prelude.Debug,
  mapLeft,
  justIf,
  tuple,
  undefined,
  unit,
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
import Exon (exon)
import GHC.Err (undefined)
import Relude hiding (Type, ask, asks, get, gets, hoistEither, hoistMaybe, local, modify, put, state, undefined)
import System.IO.Error (ioError, userError)
import Ribosome.Prelude.Debug

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

justIf :: Bool -> a -> Maybe a
justIf cond a =
  bool Nothing (Just a) cond
