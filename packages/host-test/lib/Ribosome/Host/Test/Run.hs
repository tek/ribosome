module Ribosome.Host.Test.Run where

import qualified Chronos
import Conc (Gates, interpretGates, interpretMaskFinal, interpretRace)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Debug, Trace, Warn), interpretLogStderrLevelConc)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos, interpretTimeChronosConstant)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), runTestAuto)
import Polysemy.Test.Data.TestError (SkipTestDefaultValue)
import Time (mkDatetime)

import Ribosome.Host.Data.BootError (BootError (..))
import Ribosome.Host.Data.HostConfig (setStderr)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (HostEmbedStack, embedNvim, embedNvim_)
import Ribosome.Host.IOStack (LogConfStack, interpretLogConfStack)
import Ribosome.Host.Test.Data.TestConfig (TestConfig (..))

type TestIOStack =
  [
    Log,
    Mask,
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
  SkipTestDefaultValue a =>
  Sem TestIOStack a ->
  TestT IO a
runUnitTest =
  runTestAuto .
  mapError (TestError . (.unBootError)) .
  asyncToIOFinal .
  interpretRace .
  interpretGates .
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
  SkipTestDefaultValue a =>
  TestConfig ->
  Sem TestStack a ->
  TestT IO a
runTestConf conf =
  runUnitTest .
  runTestLogConf conf

runTest ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem TestStack a ->
  TestT IO a
runTest =
  runTestConf def

runTestLevel ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Severity ->
  Sem TestStack a ->
  TestT IO a
runTestLevel level =
  runTestConf def { host = setStderr level def }

runTestDebug ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem TestStack a ->
  TestT IO a
runTestDebug =
  runTestLevel Debug

runTestTrace ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem TestStack a ->
  TestT IO a
runTestTrace =
  runTestLevel Trace

embedTestConf ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  TestConfig ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTestConf conf handlers =
  runTestConf conf .
  embedNvim handlers

embedTest ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTest =
  embedTestConf def

embedTestLevel ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Severity ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTestLevel level =
  embedTestConf def { host = setStderr level def }

embedTestDebug ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTestDebug =
  embedTestLevel Debug

embedTestTrace ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTestTrace =
  embedTestLevel Trace

embedTest_ ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTest_ =
  runTest .
  embedNvim_

embedTestDebug_ ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTestDebug_ =
  embedTestDebug mempty

embedTestTrace_ ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem (Rpc : EmbedTestStack) a ->
  TestT IO a
embedTestTrace_ =
  embedTestTrace mempty
