module Ribosome.Test.WatcherTest where

import qualified Data.Map.Strict as Map (singleton)
import Data.MessagePack (Object)
import Hedgehog ((===))
import Neovim (Plugin(..))
import TestError (RiboTest, handleTestError)

import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimGetVar)
import Ribosome.Plugin (riboPlugin)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import Ribosome.Test.Run (UnitTest)

changed :: NvimE e m => Object -> m ()
changed _ =
  setVar "number" =<< ((+) (1 :: Int)) <$> vimGetVar "number"

varWatcherPlugin :: IO (Plugin (Ribosome ()))
varWatcherPlugin = do
  env <- newRibosome "varwatcher" ()
  return $ riboPlugin "varwatcher" env [] [] handleTestError (Map.singleton "trigger" changed)

varWatcherSpec :: RiboTest ()
varWatcherSpec = do
  setVar "number" (10 :: Int)
  setVar "trigger" (5 :: Int)
  await ((10 :: Int) ===) (vimGetVar "number")
  await ((5 :: Int) ===) (vimGetVar "trigger")
  doautocmd True "CmdlineLeave"
  doautocmd True "CmdlineLeave"
  await ((11 :: Int) ===) (vimGetVar "number")
  setVar "trigger" (6 :: Int)
  await ((6 :: Int) ===) (vimGetVar "trigger")
  doautocmd True "CmdlineLeave"
  await ((12 :: Int) ===) (vimGetVar "number")

test_varWatcher :: UnitTest
test_varWatcher = do
  plug <- liftIO varWatcherPlugin
  integrationSpecDef plug varWatcherSpec
