module Ribosome.Host.Test.CommandBangTest where

import Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Data (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Bang (Bang (Bang, NoBang))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (Report, resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

bang ::
  Members [Rpc !! RpcError, Stop Report] r =>
  Bang ->
  Int64 ->
  Sem r ()
bang = \case
  Bang ->
    \ i -> resumeReport (nvimSetVar @[_] var [toMsgpack True, toMsgpack i])
  NoBang ->
    \ i -> resumeReport (nvimSetVar @[_] var [toMsgpack False, toMsgpack i])

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Bang" Sync (bang @(Stop Report : r))
  ]

test_bang :: UnitTest
test_bang =
  runTest $ interpretAtomic 0 $ embedNvim handlers do
    nvimCommand "Bang! 9"
    assertJust @(_, Int) (True, 9) =<< nvimGetVar var
    nvimCommand "Bang 10"
    assertJust @(_, Int) (False, 10) =<< nvimGetVar var
