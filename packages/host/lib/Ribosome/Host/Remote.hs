module Ribosome.Host.Remote where

import Polysemy.Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import Polysemy.Process (Process, interpretProcessCurrent)
import Time (GhcTime)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Ribosome.Host.Data.HostError (HostError)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.Embed (CoreDeps, interpretCoreDeps)
import Ribosome.Host.Interpreter.Errors (interpretErrors)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Log (interpretDataLogRpc)
import Ribosome.Host.Interpreter.Process (interpretProcessInputCereal, interpretProcessOutputCereal)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpc)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)

interpretRpcRemote ::
  Members [Error BootError, Log, Resource, Async, Race, Embed IO] r =>
  Member (Responses RequestId Response !! RpcError) r =>
  InterpretersFor [Rpc !! RpcError, Process RpcMessage (Either Text RpcMessage)] r
interpretRpcRemote =
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcessCurrent def .
  raiseUnder2 .
  resumeHoistError (BootError . show @Text) .
  interpretRpc .
  insertAt @2

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

interpretHostRemoteCore ::
  Members [UserError, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor CoreRemoteStack r
interpretHostRemoteCore =
  interpretErrors .
  interpretEventsChan @RpcMessage .
  interpretEventsChan @Event .
  interpretResponses .
  interpretRpcRemote .
  interpretDataLogRpc

interpretHostRemote ::
  Members [Error BootError, GhcTime, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  InterpretersFor RemoteStack r
interpretHostRemote conf =
  interpretCoreDeps conf .
  interpretUserErrorInfo .
  interpretHostRemoteCore

runNvimPlugin ::
  Members [Error BootError, GhcTime, Resource, Async, Race, Embed IO, Final IO] r =>
  HostConfig ->
  InterpreterFor (Handlers !! HandlerError) (RemoteStack ++ r) ->
  Sem r ()
runNvimPlugin conf handlers =
  interpretHostRemote conf (handlers runHost)
