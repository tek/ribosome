module Ribosome.Host.Unit.Run where

import qualified Chronos
import Conc (
  Gates,
  interpretGates,
  interpretMaskFinal,
  interpretRace,
  interpretUninterruptibleMaskFinal,
  )
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Trace), interpretLogStderrConc)
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Time (mkDatetime)

import Ribosome.Host.Data.BootError (BootError (unBootError))
import Ribosome.Host.Data.HostConfig (HostConfig, setStderr)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (HostEmbedStack, embedNvim, embedNvim_)
import Ribosome.Host.IOStack (LogConfStack, interpretLogConfStack)

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
  mapError (TestError . unBootError) .
  asyncToIOFinal .
  interpretRace .
  interpretGates .
  interpretUninterruptibleMaskFinal .
  interpretMaskFinal .
  interpretLogStderrConc

runTestConf ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  InterpretersFor TestConfStack r
runTestConf conf =
  interpretTimeChronos .
  interpretLogConfStack conf

runTest ::
  HasCallStack =>
  Sem TestStack () ->
  UnitTest
runTest =
  runUnitTest .
  runTestConf def

runTestTrace ::
  HasCallStack =>
  Sem TestStack () ->
  UnitTest
runTestTrace =
  runUnitTest .
  runTestConf (setStderr Trace def)

embedTestConf ::
  HasCallStack =>
  HostConfig ->
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTestConf conf handlers =
  runUnitTest .
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
