module Ribosome.Test.Run where

import Data.Time (UTCTime)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhcConstant, mkDatetime)

import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvim, embedNvim_)

type TestStack =
  [GhcTime, Race, Async, Error Text, Test, Fail, Error TestError, Hedgehog IO, Error Failure, Embed IO, Resource, Final IO]

type EmbedTestStack =
  EmbedStack ++ TestStack

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
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest handlers =
  runTest .
  embedNvim handlers

embedTest_ ::
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest_ =
  runTest .
  embedNvim_

rpcError ::
  Members [eff !! RpcError, Error HandlerError] r =>
  InterpreterFor eff r
rpcError =
  resumeHoistError (HandlerError.simple . unRpcError)
