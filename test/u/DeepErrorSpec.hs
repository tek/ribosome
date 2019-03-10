{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module DeepErrorSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.Except (runExceptT)
import Test.Framework

import Ribosome.Control.Monad.DeepError (MonadDeepError(throwHoist))
import Ribosome.Data.Deep (deepPrisms)

newtype Err1 =
  Err1 Int
  deriving (Eq, Show)

newtype Err2 =
  Err2 Int
  deriving (Eq, Show)

data Err =
  ErrC Err1
  |
  ErrC1 Err2
  deriving (Eq, Show)

deepPrisms ''Err

newtype MiddleOther =
  MiddleOther Int
  deriving (Eq, Show)

data MiddleErr =
  MiddleErrC Err
  |
  MiddleErrOther MiddleOther
  deriving (Eq, Show)

deepPrisms ''MiddleErr

newtype MainOther =
  MainOther Int
  deriving (Eq, Show)

data MainErr =
  MainErrC MiddleErr
  |
  MainErrOther MainOther
  deriving (Eq, Show)

deepPrisms ''MainErr

throwDeep :: MonadDeepError e Err m => m ()
throwDeep =
  throwHoist (ErrC (Err1 5))

test_hoist :: IO ()
test_hoist = do
  a <- runExceptT throwDeep
  assertEqual (Left (MainErrC (MiddleErrC (ErrC (Err1 5))))) a
