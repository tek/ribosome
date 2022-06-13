module Ribosome.Host.Test.MaybeParamTest where

import Polysemy.Test (Hedgehog, UnitTest, assertEq)

import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Unit.Run (embedTest)

hand ::
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Handler r Int
hand _ (Just m) (Just n) =
  pure (m * n)
hand _ (Just m) Nothing =
  pure m
hand _ Nothing (Just _) =
  stop "first arg is Nothing"
hand _ Nothing Nothing =
  pure 100

callTest ::
  Members [Rpc, Hedgehog IO] r =>
  Int ->
  [Int] ->
  Sem r ()
callTest target args =
  assertEq target =<< nvimCallFunction "Fun" (toMsgpack True : (toMsgpack <$> args))

test_maybeParams :: UnitTest
test_maybeParams =
  embedTest [rpcFunction "Fun" Sync hand] do
    callTest 299 [13, 23]
    callTest 13 [13]
    callTest 100 []
