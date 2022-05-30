module Ribosome.Host.Test.Run where

import qualified Chronos
import Conc (Restoration, interpretMaskFinal)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos, interpretTimeChronosConstant)
import Polysemy.Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Time (mkDatetime)

import Ribosome.Host.Data.BootError (BootError (unBootError))
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvimConf, embedNvim_)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Test.Data.TestConfig (TestConfig (TestConfig))

type TestStack =
  [
    ChronosTime,
    Mask Restoration,
    Race,
    Async,
    Error BootError,
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
    Error Failure,
    Embed IO,
    Resource,
    Final IO
  ]

type EmbedTestStack =
  EmbedStack ++ TestStack

testTime :: Chronos.Time
testTime =
  Chronos.datetimeToTime (mkDatetime 2025 6 15 12 30 30)

runTestConf ::
  Bool ->
  Sem TestStack () ->
  UnitTest
runTestConf freezeTime =
  runTestAuto .
  mapError (TestError . unBootError) .
  asyncToIOFinal .
  interpretRace .
  interpretMaskFinal .
  (if freezeTime then interpretTimeChronosConstant testTime else interpretTimeChronos)

runTest ::
  Sem TestStack () ->
  UnitTest
runTest =
  runTestConf False

embedTestConf ::
  TestConfig ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestConf (TestConfig freeze conf) handlers =
  runTestConf freeze .
  embedNvimConf conf (interpretHandlers handlers)

embedTest ::
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest =
  embedTestConf def

embedTest_ ::
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest_ =
  runTestConf False .
  embedNvim_

rpcError ::
  Members [Rpc !! RpcError, Stop HandlerError] r =>
  InterpreterFor Rpc r
rpcError =
  resumeHoist (HandlerError.simple . unRpcError)
