module Ribosome.Host.Test.Run where

import qualified Chronos
import Conc (Gates, interpretGates, interpretMaskFinal, interpretRace, interpretUninterruptibleMaskFinal)
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Debug, Trace, Warn), interpretLogStderrLevelConc)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos, interpretTimeChronosConstant)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Time (mkDatetime)

import Ribosome.Host.Data.BootError (BootError (unBootError))
import Ribosome.Host.Data.HostConfig (setStderr)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (HostEmbedStack, embedNvim, embedNvim_)
import Ribosome.Host.IOStack (LogConfStack, interpretLogConfStack)
import Ribosome.Host.Test.Data.TestConfig (TestConfig (TestConfig), host)

type TestIOStack =
  [
    Log,
    Mask,
    UninterruptibleMask,
    Gates,
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

type TestConfStack =
  LogConfStack ++ '[ChronosTime]

type TestStack =
  TestConfStack ++ TestIOStack

type EmbedTestStack =
  HostEmbedStack ++ TestStack

testTime :: Chronos.Time
testTime =
  Chronos.datetimeToTime (mkDatetime 2025 6 15 12 30 30)

runUnitTest ::
  HasCallStack =>
  Sem TestIOStack () ->
  UnitTest
runUnitTest =
  runTestAuto .
  mapError (TestError . (.unBootError)) .
  asyncToIOFinal .
  interpretRace .
  interpretGates .
  interpretUninterruptibleMaskFinal .
  interpretMaskFinal .
  interpretLogStderrLevelConc (Just Warn)

runTestLogConf ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  TestConfig ->
  InterpretersFor TestConfStack r
runTestLogConf (TestConfig freezeTime conf) =
  (if freezeTime then interpretTimeChronosConstant testTime else interpretTimeChronos) .
  interpretLogConfStack conf

runTestConf ::
  HasCallStack =>
  TestConfig ->
  Sem TestStack () ->
  UnitTest
runTestConf conf =
  runUnitTest .
  runTestLogConf conf

runTest ::
  HasCallStack =>
  Sem TestStack () ->
  UnitTest
runTest =
  runTestConf def

runTestLevel ::
  HasCallStack =>
  Severity ->
  Sem TestStack () ->
  UnitTest
runTestLevel level =
  runTestConf def { host = setStderr level def }

runTestDebug ::
  HasCallStack =>
  Sem TestStack () ->
  UnitTest
runTestDebug =
  runTestLevel Debug

runTestTrace ::
  HasCallStack =>
  Sem TestStack () ->
  UnitTest
runTestTrace =
  runTestLevel Trace

embedTestConf ::
  HasCallStack =>
  TestConfig ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestConf conf handlers =
  runTestConf conf .
  embedNvim handlers

embedTest ::
  HasCallStack =>
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest =
  embedTestConf def

embedTestLevel ::
  HasCallStack =>
  Severity ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestLevel level =
  embedTestConf def { host = setStderr level def }

embedTestDebug ::
  HasCallStack =>
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestDebug =
  embedTestLevel Debug

embedTestTrace ::
  HasCallStack =>
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestTrace =
  embedTestLevel Trace

embedTest_ ::
  HasCallStack =>
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest_ =
  runTest .
  embedNvim_

embedTestDebug_ ::
  HasCallStack =>
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestDebug_ =
  embedTestDebug mempty

embedTestTrace_ ::
  HasCallStack =>
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestTrace_ =
  embedTestTrace mempty
