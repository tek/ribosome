module Ribosome.Host.Remote where

import Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import Polysemy.Process (Process, interpretProcessCurrent)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.HostError (HostError)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.Embed (CoreDeps, interpretCoreDeps)
import Ribosome.Host.IOStack (IOStack, runIOStack)
import Ribosome.Host.Interpreter.Errors (interpretErrors)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Log (interpretDataLogRpc)
import Ribosome.Host.Interpreter.Process (interpretProcessInputCereal, interpretProcessOutputCereal)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpc)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)

type CoreRemoteStack =
  [
    DataLog HostError,
    Rpc !! RpcError,
    Process RpcMessage (Either Text RpcMessage),
    Responses RequestId Response !! RpcError,
    ChanEvents Event,
    ChanConsumer Event,
    ChanEvents RpcMessage,
    ChanConsumer RpcMessage,
    Errors
  ]

type RemoteStack =
  CoreRemoteStack ++ UserError : CoreDeps

type HostIOStack =
  RemoteStack ++
  IOStack

interpretRpcRemote ::
  Members IOStack r =>
  Members [Responses RequestId Response !! RpcError, Log] r =>
  InterpretersFor [Rpc !! RpcError, Process RpcMessage (Either Text RpcMessage)] r
interpretRpcRemote =
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcessCurrent def .
  raiseUnder2 .
  resumeHoistError (BootError . show @Text) .
  interpretRpc .
  insertAt @2

interpretHostRemoteCore ::
  Members IOStack r =>
  Members [UserError, Log] r =>
  InterpretersFor CoreRemoteStack r
interpretHostRemoteCore =
  interpretErrors .
  interpretEventsChan @RpcMessage .
  interpretEventsChan @Event .
  interpretResponses .
  interpretRpcRemote .
  interpretDataLogRpc

interpretHostRemote ::
  Members IOStack r =>
  HostConfig ->
  InterpretersFor RemoteStack r
interpretHostRemote conf =
  interpretCoreDeps conf .
  interpretUserErrorInfo .
  interpretHostRemoteCore

runNvimHost ::
  Members IOStack r =>
  HostConfig ->
  InterpreterFor (Handlers !! HandlerError) (RemoteStack ++ r) ->
  Sem r ()
runNvimHost conf handlers =
  interpretHostRemote conf (handlers runHost)

runNvimHostIO ::
  HostConfig ->
  InterpreterFor (Handlers !! HandlerError) HostIOStack ->
  IO ()
runNvimHostIO conf handlers =
  runIOStack (runNvimHost conf handlers)

runNvimHostHandlersIO ::
  HostConfig ->
  [RpcHandler HostIOStack] ->
  IO ()
runNvimHostHandlersIO conf handlers =
  runNvimHostIO conf (interpretHandlers handlers)
