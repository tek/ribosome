module Ribosome.Host.Remote where

import Polysemy.Conc (ChanConsumer, interpretEventsChan)
import Polysemy.Process (Process, interpretProcessCurrent)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler, hoistRpcHandler)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.Interpreter.Errors (interpretErrors)
import Ribosome.Host.Interpreter.Process (interpretProcessInputCereal, interpretProcessOutputCereal)
import Ribosome.Host.Interpreter.RequestHandler (runRequestHandler)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpcMsgpack)

interpretRpcMsgpackRemote ::
  Members [Error BootError, Log, Resource, Async, Race, Embed IO] r =>
  Member (Responses RequestId Response !! RpcError) r =>
  InterpretersFor [Rpc !! RpcError, Process RpcMessage (Either Text RpcMessage)] r
interpretRpcMsgpackRemote =
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcessCurrent def .
  resumeHoistError (BootError . show @Text) .
  interpretRpcMsgpack .
  insertAt @2

type RemoteStack =
  [
    Rpc !! RpcError,
    ChanConsumer Event,
    Errors
  ]

runNvimPlugin ::
  Members [UserError, Error BootError, Log, Resource, Async, Race, Embed IO] r =>
  [RpcHandler (RemoteStack ++ r)] ->
  Sem r ()
runNvimPlugin =
  interpretErrors .
  interpretEventsChan .
  interpretResponses .
  interpretRpcMsgpackRemote .
  runRequestHandler .
  fmap (hoistRpcHandler (insertAt @2))
