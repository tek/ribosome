module Ribosome.Host.Test.CommandRegisterTest where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.CommandRegister (CommandRegister (CommandRegister))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Test.Run (embedNvim, rpcError, runTest)

var :: Text
var =
  "test_var"

reg ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  CommandRegister ->
  Sem r ()
reg (CommandRegister r) =
  rpcError (nvimSetVar var r)

regHandlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
regHandlers =
  [
    rpcCommand "Register" Sync (reg @(Error HandlerError : r))
  ]

test_register :: UnitTest
test_register =
  runTest $ interpretAtomic 0 $ embedNvim regHandlers do
    nvimCommand "Register x"
    assertJust @Text "x" =<< nvimGetVar var
