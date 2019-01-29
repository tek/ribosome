module Ribosome.Control.Monad.Trans.Unlift(
  MonadUnlift(..),
) where

import Control.Monad.Trans.Except (ExceptT, mapExceptT)

class MonadUnlift t where
  unlift :: Monad m => (m a -> m b) -> t m a -> t m b

instance MonadUnlift (ExceptT e) where
  unlift f =
    mapExceptT (>>= trans)
    where
      trans (Left e) = return $ Left e
      trans (Right a) = Right <$> f (return a)
