{-# OPTIONS_GHC -F -pgmF htfpp #-}

module WatcherSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Map as Map (singleton)
import Data.MessagePack (Object)
import Neovim (Neovim, Plugin(..))
import Test.Framework

import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimGetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (riboPlugin)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import TestError (handleTestError)

changed :: NvimE e m => Object -> m ()
changed _ =
  setVar "number" =<< (+(1 :: Int)) <$> vimGetVar "number"

varWatcherPlugin :: IO (Plugin (Ribosome ()))
varWatcherPlugin = do
  env <- newRibosome "varwatcher" ()
  return $ riboPlugin "varwatcher" env [] [] handleTestError (Map.singleton "trigger" changed)

varWatcherSpec :: ExceptT RpcError (Neovim ()) ()
varWatcherSpec = do
  setVar "number" (10 :: Int)
  setVar "trigger" (5 :: Int)
  doautocmd "CmdlineLeave"
  doautocmd "CmdlineLeave"
  setVar "trigger" (6 :: Int)
  doautocmd "CmdlineLeave"
  await (gassertEqual (12 :: Int)) (vimGetVar "number")

test_varWatcher :: IO ()
test_varWatcher = do
  plug <- varWatcherPlugin
  integrationSpecDef plug varWatcherSpec
