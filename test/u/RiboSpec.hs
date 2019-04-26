{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module RiboSpec (htf_thisModulesTests) where

import Data.Default (def)
import Test.Framework

import Ribosome.Control.Monad.Ribo (RiboN)
import Ribosome.Test.Unit (unitSpec)
import TestError (TestError)

newtype RMState =
  RMState Int
  deriving (Eq, Show)

deepLenses ''RMState

riboModifySpec :: RiboN RMState TestError ()
riboModifySpec = do
  modify modi
  return ()
  where
    modi (RMState i) = RMState (i + 5)

test_riboModify :: IO ()
test_riboModify =
  unitSpec def (RMState 1) riboModifySpec
