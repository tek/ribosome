module Ribosome.Host.Test.AsyncTest where

import Conc (interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Host.Api.Data (nvimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Unit.Run (runTest)

hand ::
  Member (Sync Int) r =>
  Int ->
  Handler r ()
hand =
  void . Sync.putWait (Seconds 5)

handlers ::
  ∀ r .
  Member (Sync Int) r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "Fun" Async hand
  ]

test_async :: UnitTest
test_async =
  runTest $ interpretSync $ embedNvim handlers do
    Rpc.notify (nvimCallFunction @() "Fun" [toMsgpack (47 :: Int)])
    assertJust 47 =<< Sync.takeWait (Seconds 1)
