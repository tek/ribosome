module Ribosome.Host.Test.Run where

import Data.Time (UTCTime)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhcConstant, mkDatetime)

import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.RpcDef (RpcDef)
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, runNvimPluginEmbed)

type TestStack =
  [GhcTime, Error Text, Test, Fail, Error TestError, Hedgehog IO, Embed IO, Resource, Final IO]

type EmbedTestStack r =
  EmbedStack ++ r ++ TestStack

testTime :: UTCTime
testTime =
  mkDatetime 2025 6 15 12 30 30

embedTestWith ::
  Members [Resource, Error Text, Embed IO, Final IO] (r ++ TestStack) =>
  [RpcDef (Rpc !! RpcError : r ++ TestStack)] ->
  InterpretersFor r TestStack ->
  Sem (EmbedTestStack r) () ->
  UnitTest
embedTestWith handlers extra =
  runTestAuto .
  mapError TestError .
  interpretTimeGhcConstant testTime .
  extra .
  runNvimPluginEmbed handlers

runTest ::
  Sem TestStack () ->
  UnitTest
runTest =
  runTestAuto .
  mapError TestError .
  interpretTimeGhcConstant testTime

embedNvim ::
  Members [Error Text, Resource, Embed IO, Final IO] r =>
  [RpcDef (Rpc !! RpcError : r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvim handlers =
  runNvimPluginEmbed handlers .
  resumeHoistError @_ @Rpc (show @Text)

embedTest ::
  [RpcDef (Rpc !! RpcError : TestStack)] ->
  Sem (Rpc : EmbedTestStack '[]) () ->
  UnitTest
embedTest handlers =
  runTest .
  embedNvim handlers

rpcError ::
  Members [eff !! RpcError, Error HandlerError] r =>
  InterpreterFor eff r
rpcError =
  resumeHoistError (HandlerError . unRpcError)
