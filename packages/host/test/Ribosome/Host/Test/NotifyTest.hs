module Ribosome.Host.Test.NotifyTest where

import Polysemy.Conc (interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Host.Api.Data (nvimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Test.Run (embedNvim, runTest)

hand ::
  Members [Rpc !! RpcError, Sync Int, Error HandlerError] r =>
  Int ->
  Sem r ()
hand =
  void . Sync.putWait (Seconds 5)

handlers ::
  ∀ r .
  Members [Sync Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "Fun" Async (hand @(Error HandlerError : r))
  ]

test_function :: UnitTest
test_function =
  runTest $ interpretSync $ embedNvim handlers do
    Rpc.notify (nvimCallFunction "Fun" [toMsgpack i])
    assertJust i =<< Sync.takeWait (Seconds 5)
  where
    i =
      13 :: Int
