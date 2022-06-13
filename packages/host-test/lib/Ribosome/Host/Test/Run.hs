module Ribosome.Host.Test.Run where

import qualified Chronos
import Conc (Restoration, interpretMaskFinal, interpretRace, interpretUninterruptibleMaskFinal)
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Trace), interpretLogStderrConc)
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
    Mask Restoration,
    UninterruptibleMask Restoration,
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
  mapError (TestError . unBootError) .
  asyncToIOFinal .
  interpretRace .
  interpretUninterruptibleMaskFinal .
  interpretMaskFinal .
  interpretLogStderrConc

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

runTestTrace ::
  HasCallStack =>
  Sem TestStack () ->
  UnitTest
runTestTrace =
  runTestConf def { host = setStderr Trace def }

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

embedTest_ ::
  HasCallStack =>
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest_ =
  runTest .
  embedNvim_
