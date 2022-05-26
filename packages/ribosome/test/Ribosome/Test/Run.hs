module Ribosome.Test.Run where

import Data.MessagePack (Object)
import Data.Time (UTCTime)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhcConstant, mkDatetime)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (PluginHandler, PluginStack, embedNvimPlugin)
import Ribosome.Host.Data.BootError (BootError (unBootError))
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError (unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (EmbedStack, embedNvim, embedNvim_)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)

type TestStack =
  [
    GhcTime,
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

type PluginTestStack =
  PluginStack ++ TestStack

testTime :: UTCTime
testTime =
  mkDatetime 2025 6 15 12 30 30

runTest ::
  Sem TestStack () ->
  UnitTest
runTest =
  runTestAuto .
  mapError (TestError . unBootError) .
  asyncToIOFinal .
  interpretRace .
  interpretTimeGhcConstant testTime

embedTest ::
  [RpcHandler EmbedTestStack] ->
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest handlers =
  runTest .
  embedNvim (interpretHandlers handlers)

embedTest_ ::
  Sem (Rpc : EmbedTestStack) () ->
  UnitTest
embedTest_ =
  runTest .
  embedNvim_

embedPluginTest ::
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (Rpc : PluginTestStack) () ->
  UnitTest
embedPluginTest maps vars handlers =
  runTest .
  embedNvimPlugin "test" maps vars handlers

embedPluginTest_ ::
  Sem (Rpc : PluginTestStack) () ->
  UnitTest
embedPluginTest_ =
  embedPluginTest mempty mempty mempty

rpcError ::
  Members [eff !! RpcError, Stop HandlerError] r =>
  InterpreterFor eff r
rpcError =
  resumeHoist (HandlerError.simple . unRpcError)
