{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Orphans where

import Control.Monad.Catch (MonadCatch(..), MonadMask(..))
import Neovim.Context.Internal (Neovim(..))

deriving instance MonadCatch (Neovim e)
deriving instance MonadMask (Neovim e)
