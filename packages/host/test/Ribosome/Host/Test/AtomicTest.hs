module Ribosome.Host.Test.AtomicTest where

import Data.MessagePack (Object)
import qualified Polysemy.Conc as Race
import Polysemy.Test (UnitTest, assert, assertJust, (===))
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Host.Api.Data (nvimGetOption, nvimGetVar)
import Ribosome.Host.Api.Effect (nvimSetVar)
import Ribosome.Host.Class.Msgpack.Array (MsgpackArray (msgpackArray))
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Request (Request (Request), TrackedRequest (TrackedRequest))
import Ribosome.Host.Data.RpcCall (RpcCall (RpcPure))
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

test_atomic :: UnitTest
test_atomic =
  embedTest_ do
    subscribe @RpcMessage do
      nvimSetVar "a" (3 :: Int)
      nvimSetVar "b" (7 :: Int)
      (modi, a, b, c) <- Rpc.sync do
        able <- nvimGetOption "modifiable"
        modded <- nvimGetOption "modified"
        c <- RpcPure 11
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
      assertJust (req 3 (Request "nvim_call_atomic" atomicPayload)) =<< tryConsume
      assertJust (req 4 (Request "nvim_get_option" (msgpackArray ("modifiable" :: Text)))) =<< tryConsume
  where
    tryConsume =
      Race.timeoutMaybe (Seconds 5) consume
    req n r =
      RpcMessage.Request (TrackedRequest n r)
