{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module LocalSpec(
  htf_thisModulesTests,
) where

import Control.Lens (makeClassy)
import Control.Monad.State.Class (MonadState(put, get))
import Data.Default (def)
import Test.Framework

import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, local)
import Ribosome.Test.Unit (unitSpecR)
import Test ()

newtype Inner =
  Inner Int
  deriving (Eq, Show)

newtype Outer =
  Outer { _inn :: Inner }
  deriving (Eq, Show)

makeClassy ''Outer

inner :: MonadState Inner m => m Int
inner = do
  put (Inner 9)
  return 5

outerM :: Ribo Outer (ConcNvimS Outer) Int
outerM = local inn inner

ou :: Outer
ou = Outer (Inner 1)

localSpec :: Ribo Outer (ConcNvimS Outer) ()
localSpec = do
  a <- outerM
  s <- get
  gassertEqual 5 a
  gassertEqual (Outer (Inner 9)) s

test_local :: IO ()
test_local =
  unitSpecR def ou localSpec
