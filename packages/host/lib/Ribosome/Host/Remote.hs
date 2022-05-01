module Ribosome.Host.Remote where

import Data.MessagePack (Object)
import Polysemy.Process (Process, interpretProcessCurrent)
import Polysemy.Process.Data.ProcessError (ProcessError)

import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.RpcDef (RpcDef, hoistRpcDef)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Process (interpretProcessInputCereal, interpretProcessOutputCereal)
import Ribosome.Host.Interpreter.RequestHandler (runRequestHandler)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpcMsgpack)

interpretRpcMsgpackRemote ::
  Members [Log, Resource, Async, Race, Embed IO] r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Error ProcessError] r =>
  InterpretersFor [Rpc !! RpcError, Process Object (Either Text Object)] r
interpretRpcMsgpackRemote =
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcessCurrent def .
  resumeError .
  interpretRpcMsgpack .
  insertAt @2

runNvimPlugin ::
  Members [Error ProcessError, Error Text, Log, Resource, Async, Race, Embed IO] r =>
  [RpcDef (Rpc !! RpcError : r)] ->
  Sem r ()
runNvimPlugin =
  interpretResponses .
  interpretRpcMsgpackRemote .
  runRequestHandler .
  fmap (hoistRpcDef (insertAt @2))
