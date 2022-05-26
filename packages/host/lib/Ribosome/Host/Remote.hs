module Ribosome.Host.Remote where

import Polysemy.Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)
import Polysemy.Process (Process, interpretProcessCurrent)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.Interpreter.Errors (interpretErrors)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process (interpretProcessInputCereal, interpretProcessOutputCereal)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpcMsgpack)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)

type RpcStack =
  [
    Rpc !! RpcError,
    Process RpcMessage (Either Text RpcMessage)
  ]

type RpcRemoteStack =
  RpcStack ++ '[Responses RequestId Response !! RpcError]

interpretRpcMsgpackRemote ::
  Members [Error BootError, Log, Resource, Async, Race, Embed IO] r =>
  Member (Responses RequestId Response !! RpcError) r =>
  InterpretersFor RpcStack r
interpretRpcMsgpackRemote =
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcessCurrent def .
  raiseUnder2 .
  resumeHoistError (BootError . show @Text) .
  interpretRpcMsgpack .
  insertAt @2

type BasicRemoteStack =
  RpcRemoteStack ++ [
    ChanEvents Event,
    ChanConsumer Event,
    ChanEvents RpcMessage,
    ChanConsumer RpcMessage,
    Errors
  ]

type RemoteStack =
  BasicRemoteStack ++ [
    UserError,
    Log
  ]

interpretRemoteStack ::
  Members [Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor BasicRemoteStack r
interpretRemoteStack =
  interpretErrors .
  interpretEventsChan @RpcMessage .
  interpretEventsChan @Event .
  interpretResponses .
  interpretRpcMsgpackRemote

interpretRemoteStackLog ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  Severity ->
  InterpretersFor RemoteStack r
interpretRemoteStackLog logLevel =
  interpretLogStdoutLevelConc (Just logLevel) .
  interpretUserErrorInfo .
  interpretRemoteStack

runNvimPlugin ::
  Members [UserError, Error BootError, Log, Resource, Async, Race, Embed IO, Final IO] r =>
  InterpreterFor (Handlers !! HandlerError) (RemoteStack ++ r) ->
  Sem r ()
runNvimPlugin handlers =
  interpretRemoteStackLog Warn (handlers runHost)
