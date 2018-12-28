module Ribosome.Internal.IO(
  retypeNeovim,
  forkNeovim,
) where

import GHC.Conc.Sync (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT, withReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Functor (void)
import Neovim.Context.Internal (Neovim(..), Config(..), retypeConfig, runNeovim)

-- try using Contravariant with this
retypeNeovim :: (e0 -> e1) -> Neovim e1 a -> Neovim e0 a
retypeNeovim transform thunk = do
  env <- Neovim ask
  liftIO $ runReaderT (withReaderT (newEnv env) $ runResourceT $ unNeovim thunk) env
  where
    newEnv = retypeConfig . transform . customConfig

forkNeovim :: Neovim e () -> Neovim e ()
forkNeovim thunk = do
  env <- Neovim ask
  _ <- liftIO $ forkIO $ void $ runNeovim env thunk
  return ()
