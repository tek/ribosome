module Ribosome.Host.Test.AtomicTest where

import qualified Conc as Race
import Conc (Consume)
import Data.MessagePack (Object)
import Polysemy.Test (Hedgehog, UnitTest, assert, assertJust, (===))
import Polysemy.Time (Seconds (Seconds))

import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Data (nvimGetOption, nvimGetVar, nvimSetOption)
import Ribosome.Host.Api.Data (nvimSetVar)
import Ribosome.Host.Class.Msgpack.Array (MsgpackArray (msgpackArray))
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Request (Request (Request), RequestId, TrackedRequest (TrackedRequest))
import Ribosome.Host.Data.RpcCall (RpcCall)
import qualified Ribosome.Host.Data.RpcMessage as RpcMessage
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Unit.Run (embedTest_)

atomicPayload :: [Object]
atomicPayload =
  [
    msgpackArray
    (Request "nvim_get_option" [toMsgpack @Text "modifiable"])
    (Request "nvim_get_option" [toMsgpack @Text "modified"])
    (Request "nvim_get_var" [toMsgpack @Text "a"])
    (Request "nvim_get_var" [toMsgpack @Text "b"])
  ]

tryConsume ::
  Members [Consume a, Race] r =>
  Sem r (Maybe a)
tryConsume =
  Race.timeoutMaybe (Seconds 5) consume

req :: RequestId -> Request -> RpcMessage
req n r =
  RpcMessage.Request (TrackedRequest n r)

assertReq ::
  Members [Consume RpcMessage, Hedgehog IO, Race] r =>
  RequestId ->
  Request ->
  Sem r ()
assertReq n r =
  assertJust (req n r) =<< tryConsume

test_atomic :: UnitTest
test_atomic =
  embedTest_ do
    subscribe @RpcMessage do
      nvimSetVar "a" (3 :: Int)
      nvimSetVar "b" (7 :: Int)
      (modi, a, b, c) <- Rpc.sync do
        able <- nvimGetOption "modifiable"
        modded <- nvimGetOption "modified"
        c <- pure 11
        a <- nvimGetVar "a"
        b <- nvimGetVar "b"
        pure (able && not modded, a, b, c)
      assert modi
      (3 :: Int) === a
      (7 :: Int) === b
      (11 :: Int) === c
      assert . not =<< Rpc.sync (not <$> nvimGetOption "modifiable")
      void tryConsume
      void tryConsume
      assertReq 3 (Request "nvim_call_atomic" atomicPayload)
      assertReq 4 (Request "nvim_get_option" (msgpackArray ("modifiable" :: Text)))
      d <- Rpc.sync (pure (1 :: Int))
      1 === d
      nvimSetVar "a" (3 :: Int)
      assertReq 5 (Request "nvim_set_var" [toMsgpack @Text "a", toMsgpack @Int 3])

monadicPayload1 :: [Object]
monadicPayload1 =
  [
    msgpackArray
    (Request "nvim_get_option" [toMsgpack @Text "modifiable"])
    (Request "nvim_get_option" [toMsgpack @Text "modified"])
    (Request "nvim_get_var" [toMsgpack @Text "a"])
  ]

monadicPayload2 :: [Object]
monadicPayload2 =
  [
    msgpackArray
    (Request "nvim_set_var" [toMsgpack @Text "b", toMsgpack @Int 3])
    (Request "nvim_get_var" [toMsgpack @Text "b"])
    (Request "nvim_set_option" [toMsgpack @Text "modifiable", toMsgpack @Bool False])
    (Request "nvim_get_option" [toMsgpack @Text "modifiable"])
  ]

bindCall :: RpcCall (Bool, Bool, Bool, Int, Int)
bindCall = do
  (able, modded, a) <- do
    able <- nvimGetOption "modifiable"
    modded <- nvimGetOption "modified"
    a <- nvimGetVar @Int "a"
    pure (able, modded, a)
  (able', b) <- do
    Data.nvimSetVar "b" a
    b <- nvimGetVar "b"
    nvimSetOption "modifiable" (not able)
    able' <- nvimGetOption "modifiable"
    pure (able', b)
  b' <- do
    b'' <- nvimGetVar "b"
    Data.nvimSetVar "b" (b'' + 10 :: Int)
    nvimGetVar "b"
  pure (able, able', modded, b, b')

test_atomicBind :: UnitTest
test_atomicBind =
  embedTest_ do
    subscribe @RpcMessage do
      nvimSetVar "a" (3 :: Int)
      (able, able', modded, b, b') <- Rpc.sync bindCall
      void tryConsume
      assertReq 2 (Request "nvim_call_atomic" monadicPayload1)
      assertReq 3 (Request "nvim_call_atomic" monadicPayload2)
      assertReq 4 (Request "nvim_get_var" [toMsgpack @Text "b"])
      assertReq 5 (Request "nvim_set_var" [toMsgpack @Text "b", toMsgpack @Int 13])
      assertReq 6 (Request "nvim_get_var" [toMsgpack @Text "b"])
      (True, False, False, 3 :: Int, 13 :: Int) === (able, able', modded, b, b')
