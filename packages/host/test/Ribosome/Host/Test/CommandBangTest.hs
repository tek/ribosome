module Ribosome.Host.Test.CommandBangTest where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Bang (Bang (Bang, NoBang))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Test.Run (rpcError, runTest)

var :: Text
var =
  "test_var"

bang ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Bang ->
  Int64 ->
  Sem r ()
bang = \case
  Bang ->
    \ i -> rpcError (nvimSetVar @[_] var [toMsgpack True, toMsgpack i])
  NoBang ->
    \ i -> rpcError (nvimSetVar @[_] var [toMsgpack False, toMsgpack i])

bangHandlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
bangHandlers =
  [
    rpcCommand "Bang" Sync (bang @(Error HandlerError : r))
  ]

test_bang :: UnitTest
test_bang =
  runTest $ interpretAtomic 0 $ embedNvim bangHandlers do
    nvimCommand "Bang! 9"
    assertJust @(_, Int) (True, 9) =<< nvimGetVar var
    nvimCommand "Bang 10"
    assertJust @(_, Int) (False, 10) =<< nvimGetVar var
