module Ribosome.Internal.IO where

import Control.Concurrent (forkIO)
import Control.Monad.Trans.Resource (runResourceT)
import Neovim.Context.Internal (Config(..), Neovim(..), retypeConfig, runNeovim)
import qualified Control.Monad.Reader as ReaderT

retypeNeovim :: (e0 -> e1) -> Neovim e1 a -> Neovim e0 a
retypeNeovim transform thunk = do
  env <- Neovim ReaderT.ask
  liftIO $ runReaderT (withReaderT (newEnv env) $ runResourceT $ unNeovim thunk) env
  where
    newEnv = retypeConfig . transform . customConfig

forkNeovim :: Neovim e () -> Neovim e ()
forkNeovim thunk = do
  env <- Neovim ReaderT.ask
  _ <- liftIO $ forkIO $ void $ runNeovim env thunk
  return ()
