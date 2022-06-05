module Ribosome.Host.Test.CommandArgsTest where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.Args (Args (Args))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

args ::
  Members [Rpc !! RpcError, Stop HandlerError] r =>
  Args ->
  Sem r ()
args (Args a) =
  resumeHandlerError (nvimSetVar var a)

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Args" Sync (args @(Stop HandlerError : r))
  ]

test_args :: UnitTest
test_args =
  runTest $ interpretAtomic 0 $ embedNvim handlers do
    nvimCommand "Args 1 2 3 4 5"
    assertJust @Text "1 2 3 4 5" =<< nvimGetVar var
