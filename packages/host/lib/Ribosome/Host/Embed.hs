module Ribosome.Host.Embed where

import Data.Serialize (Serialize)
import Polysemy.Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)
import qualified Polysemy.Process as Process
import Polysemy.Process (Process, ProcessOptions, withProcess)
import Polysemy.Process.Data.ProcessError (ProcessError)
import System.Process.Typed (ProcessConfig, proc)

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
import Ribosome.Host.Interpreter.Process (interpretProcessCerealNative)
import Ribosome.Host.Interpreter.RequestHandler (withRequestHandler)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpcMsgpack)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)

basicCliArgs :: [String]
basicCliArgs =
  ["--embed", "-n", "-u", "NONE", "-i", "NONE", "--clean"]

embeddedProcessConfig :: ProcessConfig () () ()
embeddedProcessConfig =
  proc "nvim" basicCliArgs

interpretProcessCerealNvimEmbed ::
  Serialize a =>
  Members [Log, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process a (Either Text a)) !! ProcessError) r
interpretProcessCerealNvimEmbed options =
  interpretProcessCerealNative options embeddedProcessConfig

publishRequests ::
  ∀ res i o r a .
  Members [Process i o, Events res i] r =>
  Sem r a ->
  Sem r a
publishRequests =
  intercept @(Process i o) \case
    Process.Send msg -> do
      publish msg
      Process.send msg
    e ->
      send @(Process i o) (coerce e)

interpretRpcMsgpackProcessSingle ::
  Members [Scoped () (Process RpcMessage o) !! ProcessError, Events res RpcMessage] r =>
  Members [Responses RequestId Response !! RpcError, Error BootError, Log, Async, Embed IO] r =>
  InterpretersFor [Rpc !! RpcError, Process RpcMessage o] r
interpretRpcMsgpackProcessSingle =
  resumeHoistError (BootError . show) .
  withProcess @() .
  raiseUnder .
  publishRequests .
  interpretRpcMsgpack

type RpcStack =
  [
    Rpc !! RpcError,
    Process RpcMessage (Either Text RpcMessage),
    Scoped () (Process RpcMessage (Either Text RpcMessage)) !! ProcessError
  ]

interpretRpcMsgpackProcessNvimEmbed ::
  Members [Responses RequestId Response !! RpcError, Events res RpcMessage] r =>
  Members [Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  ProcessConfig () () () ->
  InterpretersFor RpcStack r
interpretRpcMsgpackProcessNvimEmbed conf =
  interpretProcessCerealNative def conf .
  interpretRpcMsgpackProcessSingle

interpretRpcMsgpackProcessNvimEmbedDef ::
  Members [Responses RequestId Response !! RpcError, Events res RpcMessage] r =>
  Members [Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcStack r
interpretRpcMsgpackProcessNvimEmbedDef =
  interpretProcessCerealNvimEmbed def .
  interpretRpcMsgpackProcessSingle

interpretRpcEmbed ::
  Members [Events er Event, Events res RpcMessage, UserError, Errors] r =>
  Members [Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  [RpcHandler (Rpc !! RpcError : r)] ->
  InterpreterFor (Rpc !! RpcError) r
interpretRpcEmbed handlers =
  interpretResponses .
  interpretRpcMsgpackProcessNvimEmbedDef .
  withRequestHandler (hoistRpcHandler (insertAt @2) <$> handlers) .
  insertAt @2 .
  raise

type BasicEmbedDeps =
  [
    ChanEvents Event,
    ChanConsumer Event,
    ChanEvents RpcMessage,
    ChanConsumer RpcMessage,
    Errors
  ]

type BasicEmbedStack =
  Rpc !! RpcError : BasicEmbedDeps

type EmbedStack =
  BasicEmbedStack ++ [
    UserError,
    Log
  ]

interpretBasicEmbedDeps ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  InterpretersFor BasicEmbedDeps r
interpretBasicEmbedDeps =
  interpretErrors .
  interpretEventsChan @RpcMessage .
  interpretEventsChan @Event

runHostEmbedBasic ::
  Members [Error BootError, Resource, Race, Async, UserError, Log, Embed IO, Final IO] r =>
  [RpcHandler (BasicEmbedStack ++ r)] ->
  InterpretersFor BasicEmbedStack r
runHostEmbedBasic handlers =
  interpretBasicEmbedDeps .
  interpretRpcEmbed handlers

runHostEmbedLog ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor EmbedStack r
runHostEmbedLog logLevel handlers =
  interpretLogStdoutLevelConc (Just logLevel) .
  interpretUserErrorInfo .
  runHostEmbedBasic handlers

runHostEmbed ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor EmbedStack r
runHostEmbed =
  runHostEmbedLog Warn

embedNvimBasic ::
  Members [UserError, Log, Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler (BasicEmbedStack ++ r)] ->
  InterpretersFor (Rpc : BasicEmbedStack) r
embedNvimBasic handlers =
  runHostEmbedBasic handlers .
  resumeHoistError @_ @Rpc (BootError . show @Text)

embedNvimLog ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvimLog level handlers =
  runHostEmbedLog level handlers .
  resumeHoistError @_ @Rpc (BootError . show @Text)

embedNvim ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvim handlers =
  runHostEmbed handlers .
  resumeHoistError @_ @Rpc (BootError . show @Text)

embedNvim_ ::
  Members [Error BootError, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor (Rpc : EmbedStack) r
embedNvim_ =
  embedNvim []
