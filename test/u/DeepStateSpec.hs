{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module DeepStateSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.State (execStateT)
import Test.Framework

import Ribosome.Control.Monad.DeepState (MonadDeepState(get, put))
import Ribosome.Data.DeepLenses (deepLenses)

newtype S1 =
  S1 Int
  deriving (Eq, Show)

newtype S2 =
  S2 Int
  deriving (Eq, Show)

newtype Bot =
  BotC { _botS1 :: S1 }
  deriving (Eq, Show)

deepLenses ''Bot

newtype MiddleOther =
  MiddleOther Int
  deriving (Eq, Show)

newtype MiddleS =
  MiddleSC { _middleBot :: Bot }
  deriving (Eq, Show)

deepLenses ''MiddleS

newtype MainS =
  MainSC { _mainMiddle :: MiddleS }
  deriving (Eq, Show)

deepLenses ''MainS

stateDeep :: MonadDeepState s Bot m => m ()
stateDeep = do
  (BotC (S1 a)) <- get
  put (BotC (S1 (a + 3)))

test_lens :: IO ()
test_lens = do
  a <- execStateT stateDeep (MainSC (MiddleSC (BotC (S1 5))))
  assertEqual (MainSC (MiddleSC (BotC (S1 8)))) a
