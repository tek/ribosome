module Ribosome.Host.Test.MaybeParamTest where

import Conc (interpretSync)
import Exon (exon)
import Polysemy.Test (Hedgehog, UnitTest, assertEq, assertJust)
import qualified Sync
import Time (Seconds (Seconds))

import Ribosome.Host.Api.Effect (nvimCallFunction, nvimCommand)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand, rpcFunction)
import Ribosome.Host.Unit.Run (runTest)

fun ::
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Handler r Int
fun _ (Just m) (Just n) =
  pure (m * n)
fun _ (Just m) Nothing =
  pure m
fun _ Nothing (Just _) =
  stop "first arg is Nothing"
fun _ Nothing Nothing =
  pure 100

cmd ::
  Member (Sync Int) r =>
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Handler r ()
cmd b m n =
  void . Sync.putWait (Seconds 5) =<< fun b m n

callTest ::
  Members [Rpc, Hedgehog IO] r =>
  Int ->
  [Int] ->
  Sem r ()
callTest target args =
  assertEq target =<< nvimCallFunction "Fun" (toMsgpack ("1" :: Text) : (toMsgpack <$> args))

cmdTest ::
  Members [Sync Int, Rpc, Hedgehog IO] r =>
  Int ->
  Text ->
  Sem r ()
cmdTest target args = do
  nvimCommand [exon|Com 1 #{args}|]
  assertJust target =<< Sync.takeWait (Seconds 5)

test_maybeParams :: UnitTest
test_maybeParams =
  runTest $ interpretSync $ embedNvim [rpcFunction "Fun" Sync fun, rpcCommand "Com" Async cmd] do
    callTest 299 [13, 23]
    callTest 13 [13]
    callTest 100 []
    cmdTest 299 "13 23"
    cmdTest 13 "13"
    cmdTest 100 ""
