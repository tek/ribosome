module Ribosome.Host.Test.Run where

import qualified Chronos
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime, interpretTimeChronosConstant)
import Polysemy.Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Time (mkDatetime)

import Ribosome.Host.Data.BootError (BootError (unBootError))
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvimConf, embedNvim_)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)

type TestStack =
  [
    ChronosTime,
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
  interpretTimeChronosConstant testTime

embedTestConf ::
  HostConfig ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestConf conf handlers =
  runTest .
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
  runTest .
  embedNvim_
