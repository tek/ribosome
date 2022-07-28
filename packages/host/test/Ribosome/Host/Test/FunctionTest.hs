module Ribosome.Host.Test.FunctionTest where

import Conc (interpretAtomic, interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertEq, assertJust, assertLeft, assertRight, evalMaybe)
import Polysemy.Time (Seconds (Seconds))

import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Effect (nvimCallFunction, nvimGetVar, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Bar (Bar (Bar))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

hand ::
  Members [AtomicState Int, Rpc !! RpcError] r =>
  Bar ->
  Bool ->
  Int ->
  Handler r Int
hand Bar _ n = do
  atomicGet >>= \case
    13 ->
      stop "already 13"
    _ -> do
      resumeReport (nvimSetVar var n)
      47 <$ atomicPut 13

targetError :: RpcError
targetError =
  "Vim(return):Error invoking 'function:Fun' on channel 1:\nalready 13"

callTest ::
  Member Rpc r =>
  Int ->
  Sem r Int
callTest n =
  nvimCallFunction "Fun" [toMsgpack True, toMsgpack n]

test_function :: UnitTest
test_function =
  runTest $ interpretAtomic 0 $ embedNvim [rpcFunction "Fun" Sync hand] $ interpretSync do
    nvimSetVar var (10 :: Int)
    Rpc.async (Data.nvimGetVar var) (void . Sync.putTry)
    assertRight (10 :: Int) =<< evalMaybe =<< Sync.wait (Seconds 5)
    assertEq 47 =<< callTest 23
    assertJust (23 :: Int) =<< nvimGetVar var
    assertEq 13 =<< atomicGet
    assertLeft targetError =<< resumeEither (callTest 14)
