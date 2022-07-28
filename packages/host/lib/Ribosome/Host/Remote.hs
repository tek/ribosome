module Ribosome.Host.Remote where

import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.IOStack (BasicStack, IOStack, runBasicStack)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)

type HostRemoteStack =
  RpcStack ++ RpcDeps

type HostRemoteIOStack =
  HostRemoteStack ++ BasicStack

interpretRpcDeps ::
  Members IOStack r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorInfo .
  interpretProcessCerealStdio

interpretHostRemote ::
  Members BasicStack r =>
  InterpretersFor HostRemoteStack r
interpretHostRemote =
  interpretRpcDeps .
  interpretRpcStack

runHostRemote ::
  Members BasicStack r =>
  InterpreterFor (Handlers !! Report) (HostRemoteStack ++ r) ->
  Sem r ()
runHostRemote handlers =
  interpretHostRemote (handlers runHost)

runHostRemoteIO ::
  HostConfig ->
  [RpcHandler HostRemoteIOStack] ->
  IO ()
runHostRemoteIO conf handlers =
  runBasicStack conf $
  runHostRemote (interpretHandlers handlers)
