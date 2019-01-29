{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RiboTransSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Default.Class (Default(def))
import Test.Framework
import Neovim
import UnliftIO.STM
import Ribosome.Api.Buffer
import Ribosome.Control.Monad.Trans.Ribo
import Ribosome.Test.Unit (unitSpec)

newtype Env =
  Env Int
  deriving (Eq, Show)

rib :: RiboT (ReaderT Env) Env (Doc AnsiStyle) [String]
rib = do
  a <- nvim currentBufferContent
  nvim $ vim_command' "tabnew"
  return a

riboSpec :: Ribo Env ()
riboSpec = do
  let target = ["line 1", "line 2"]
  _ <- setCurrentBufferContent target
  content <- unsafeToNeovimWith (`runReaderT` Env 6) $ mapE (const ()) rib
  liftIO $ assertEqual target content

test_riboTrans :: IO ()
test_riboTrans = do
  t <- newTVarIO (Env 5)
  unitSpec def t riboSpec
