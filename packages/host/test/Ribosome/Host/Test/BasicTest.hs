module Ribosome.Host.Test.BasicTest where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic, interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertEq, assertJust, assertLeft, assertRight, evalMaybe)
import Polysemy.Time (Seconds (Seconds))

import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Effect (nvimCallFunction, nvimGetVar, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcDef (RpcDef (RpcDef))
import Ribosome.Host.Data.RpcError (RpcError)
import qualified Ribosome.Host.Data.RpcType as RpcType
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Test.Run (embedNvim, rpcError, runTest)

var :: Text
var =
  "test_var"

hand ::
  Members [AtomicState Int, Rpc !! RpcError, Error HandlerError] r =>
  [Object] ->
  Sem r Object
hand _ = do
  atomicGet >>= \case
    13 ->
      throw "already 13"
    _ -> do
      rpcError (nvimSetVar var (toMsgpack (23 :: Int)))
      toMsgpack (47 :: Int) <$ atomicPut 13

handlers ::
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcDef r]
handlers =
  [RpcDef RpcType.Function "Test" Sync hand]

targetError :: RpcError
targetError =
  "Vim(let):Error invoking 'Test' on channel 1:\nalready 13"

callTest ::
  Member Rpc r =>
  Sem r Int
callTest =
  nvimCallFunction "Test" []

-- TODO replace all Object parameters in the API with MsgpackEncode a => a
test_basic :: UnitTest
test_basic =
  runTest $ interpretAtomic 0 $ embedNvim handlers $ interpretSync do
    nvimSetVar var (toMsgpack (10 :: Int))
    Rpc.async (Data.nvimGetVar var) (void . Sync.putTry)
    assertRight (10 :: Int) =<< evalMaybe =<< Sync.wait (Seconds 5)
    assertEq 47 =<< callTest
    assertJust (23 :: Int) =<< nvimGetVar var
    assertEq 13 =<< atomicGet
    assertLeft targetError =<< resumeEither callTest
