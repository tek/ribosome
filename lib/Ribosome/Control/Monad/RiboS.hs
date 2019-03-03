module Ribosome.Control.Monad.RiboS where
  -- (
  -- RiboS(..),
-- ) where

-- import Control.Monad.IO.Class (MonadIO)
-- import Control.Monad.State.Class (MonadState(get, put))
-- import Neovim (Neovim)
-- import UnliftIO.STM (TVar)

-- import qualified Ribosome.Control.Ribo as Ribo (state, put)
-- import Ribosome.Control.Ribosome (Ribosome)

-- type Ribo s = Neovim (Ribosome (TVar s))

-- newtype RiboS s a =
  -- RiboS { unRiboS :: Ribo s a }
  -- deriving (Functor, Applicative, Monad, MonadIO)

-- instance MonadState s (RiboS s) where
  -- get = RiboS Ribo.state
  -- put = RiboS . Ribo.put
