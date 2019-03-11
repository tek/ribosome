module Ribosome.Control.Monad.DeepState where

import qualified Control.Lens as Lens (set, view)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as MS (MonadState(get), modify)

import Ribosome.Data.DeepLenses (DeepLenses(deepLens))

class (MonadState s m, DeepLenses s s') => MonadDeepState s s' m where
  get :: m s'
  put :: s' -> m ()
  state :: (s' -> m (a, s')) -> m a

instance (MonadState s m, DeepLenses s s') => MonadDeepState s s' m where
  get = Lens.view deepLens <$> MS.get
  put = MS.modify . Lens.set deepLens
  state f = do
    s' <- get
    ~(a, s'') <- f s'
    put s''
    return a
