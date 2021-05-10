{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Test.Orphans where

import Hedgehog (TestT)
import Ribosome.Control.Monad.Ribo (NvimE)

instance (NvimE e m) => NvimE e (TestT m) where
