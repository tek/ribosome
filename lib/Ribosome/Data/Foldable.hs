module Ribosome.Data.Foldable(
  findMapMaybeM,
) where

import Control.Monad (foldM)

findMapMaybeM :: (Monad m, Foldable f) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
findMapMaybeM f fa =
  foldM evaluate Nothing fa
  where
    evaluate (Just b) _ = return (Just b)
    evaluate Nothing a = f a
