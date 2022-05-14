module Ribosome.Host.Test.Run where

import Data.Time (UTCTime)
import Polysemy.Conc (interpretRace)
import Polysemy.Log (Severity)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhcConstant, mkDatetime)

import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, runNvimPluginEmbed, runNvimPluginEmbedLog)

type TestStack =
  [GhcTime, Race, Async, Error Text, Test, Fail, Error TestError, Hedgehog IO, Embed IO, Resource, Final IO]

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

embedNvimLog ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  [RpcHandler (Rpc !! RpcError : r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvimLog level handlers =
  runNvimPluginEmbedLog level handlers .
  resumeHoistError @_ @Rpc (show @Text)

embedNvim ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler (Rpc !! RpcError : r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvim handlers =
  runNvimPluginEmbed handlers .
  resumeHoistError @_ @Rpc (show @Text)

embedNvim_ ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor (Rpc : EmbedStack) r
embedNvim_ =
  embedNvim []

embedTest ::
  [RpcHandler (Rpc !! RpcError : TestStack)] ->
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
  Members [eff !! RpcError, Error HandlerError] r =>
  InterpreterFor eff r
rpcError =
  resumeHoistError (HandlerError . unRpcError)
