module Ribosome.Host.Test.Run where

import qualified Chronos
import Conc (Restoration, interpretMaskFinal)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime, interpretTimeChronosConstant)
import Polysemy.Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Time (mkDatetime)

import Ribosome.Host.Data.BootError (BootError (unBootError))
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvim, embedNvim_)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)

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

runTest ::
  Sem TestStack () ->
  UnitTest
runTest =
  runTestAuto .
  mapError (TestError . unBootError) .
  asyncToIOFinal .
  interpretRace .
  interpretMaskFinal .
  interpretTimeChronosConstant testTime

embedTestConf ::
  HostConfig ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestConf conf handlers =
  runTest .
  embedNvim conf (interpretHandlers handlers)

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
  runTest .
  embedNvim_ def

rpcError ::
  Members [Rpc !! RpcError, Stop HandlerError] r =>
  InterpreterFor Rpc r
rpcError =
  resumeHoist (HandlerError.simple . unRpcError)
