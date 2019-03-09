{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module DeepErrorSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Foldable (traverse_)
import Test.Framework
import Language.Haskell.TH

import Ribosome.Data.DeepError

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

deepError ''Err

newtype MiddleOther =
  MiddleOther Int
  deriving (Eq, Show)

data MiddleErr =
  MiddleErrC Err
  |
  MiddleErrOther MiddleOther
  deriving (Eq, Show)

deepError ''MiddleErr

newtype MainOther =
  MainOther Int
  deriving (Eq, Show)

data MainErr =
  MainErrC MiddleErr
  |
  MainErrOther MainOther
  deriving (Eq, Show)

deepError ''MainErr

$(return [])

f :: MonadDeepError e Err m => m ()
f = throwHoist (ErrC (Err1 5))

ex :: ExceptT MainErr IO ()
ex = f

test_hoist :: IO ()
test_hoist = do
  traverse_ putStrLn $ lines $(stringE . pprint =<< deepError ''MainErr)
  a <- runExceptT ex
  assertEqual (Left (MainErrC (MiddleErrC (ErrC (Err1 5))))) a
