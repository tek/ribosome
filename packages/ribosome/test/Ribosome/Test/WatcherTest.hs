module Ribosome.Test.WatcherTest where

import Data.MessagePack (Object)
import Conc (interpretAtomic)
import Polysemy.Test (UnitTest, (===))

import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Host.Api.Effect (nvimSetVar)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Test.Wait (assertWait)
import Ribosome.Unit.Run (runTest, testHandlers)

changed ::
  Members [AtomicState Int, Rpc !! RpcError, Stop HandlerError] r =>
  Object ->
  Sem r ()
changed _ =
  atomicModify' (+ 1)

test_varWatcher :: UnitTest
test_varWatcher =
  runTest $ interpretAtomic 0 $ testHandlers mempty mempty [("trigger", changed)] do
    nvimSetVar "trigger" (4 :: Int)
    doautocmd "CmdlineLeave"
    assertWait atomicGet ((1 :: Int) ===)
    nvimSetVar "trigger" (5 :: Int)
    doautocmd "CmdlineLeave"
    doautocmd "CmdlineLeave"
    doautocmd "CmdlineLeave"
    assertWait atomicGet ((2 :: Int) ===)
    nvimSetVar "trigger" (6 :: Int)
    doautocmd "CmdlineLeave"
    assertWait atomicGet ((3 :: Int) ===)
