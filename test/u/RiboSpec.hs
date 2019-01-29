{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RiboSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor(..))
import Data.Default.Class (Default(def))
import Test.Framework
import Neovim
import UnliftIO.STM
import Ribosome.Api.Buffer
import Ribosome.Control.Monad.Ribo
import Ribosome.Test.Unit (unitSpec)

newtype Env =
  Env Int
  deriving (Eq, Show)

rib :: RiboT Env (Doc AnsiStyle) [String]
rib = do
  a <- nvim currentBufferContent
  nvim $ vim_command' "tabnew"
  return a

riboSpec :: Ribo (TVar Env) ()
riboSpec = do
  let target = ["line 1", "line 2"]
  _ <- setCurrentBufferContent target
  content <- unsafeToNeovim $ first (const ()) rib
  liftIO $ assertEqual target content

test_ribo :: IO ()
test_ribo = do
  t <- newTVarIO (Env 5)
  unitSpec def t riboSpec
