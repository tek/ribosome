module Ribosome.Host.Test.Run where

import Data.Time (UTCTime)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhcConstant, mkDatetime)

import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvim, embedNvim_)

type TestStack =
  [
    GhcTime,
    Race,
    Async,
    Error Text,
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
    Error Failure,
    Embed IO,
    Resource,
    Final IO
  ]

type EmbedTestStack r =
  EmbedStack ++ r ++ TestStack

testTime :: UTCTime
testTime =
  mkDatetime 2025 6 15 12 30 30

runTest ::
  Sem TestStack () ->
  UnitTest
runTest =
  runTestAuto .
  mapError TestError .
  asyncToIOFinal .
  interpretRace .
  interpretTimeGhcConstant testTime

embedTest ::
  [RpcHandler (EmbedTestStack '[])] ->
  Sem (Rpc : EmbedTestStack '[]) () ->
  UnitTest
embedTest handlers =
  runTest .
  embedNvim handlers

embedTest_ ::
  Sem (Rpc : EmbedTestStack '[]) () ->
  UnitTest
embedTest_ =
  runTest .
  embedNvim_

rpcError ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  InterpreterFor Rpc r
rpcError =
  resumeHoistError (HandlerError . unRpcError)
