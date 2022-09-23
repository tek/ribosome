module Ribosome.Host.Test.CommandRegisterTest where

import Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Data (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.CommandRegister (CommandRegister (CommandRegister))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

reg ::
  Member (Rpc !! RpcError) r =>
  CommandRegister ->
  Handler r ()
reg (CommandRegister r) =
  resumeReport (nvimSetVar var r)

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Register" Sync reg
  ]

test_register :: UnitTest
test_register =
  runTest $ interpretAtomic 0 $ embedNvim handlers do
    nvimCommand "Register x"
    assertJust @Text "x" =<< nvimGetVar var
