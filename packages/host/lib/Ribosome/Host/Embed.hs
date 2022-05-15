module Ribosome.Host.Embed where

import Data.Serialize (Serialize)
import Polysemy.Conc (ChanConsumer, interpretEventsChan)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)
import qualified Polysemy.Process as Process
import Polysemy.Process (Process, ProcessOptions, withProcess)
import Polysemy.Process.Data.ProcessError (ProcessError)
import System.Process.Typed (ProcessConfig, proc)

import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler, hoistRpcHandler)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Process (interpretProcessCerealNative)
import Ribosome.Host.Interpreter.RequestHandler (withRequestHandler)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpcMsgpack)

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
  Members [Responses RequestId Response !! RpcError, Error ProcessError, Log, Async, Embed IO] r =>
  InterpretersFor [Rpc !! RpcError, Process RpcMessage o] r
interpretRpcMsgpackProcessSingle =
  resumeError .
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
  Members [Error ProcessError, Log, Resource, Race, Async, Embed IO] r =>
  ProcessConfig () () () ->
  InterpretersFor RpcStack r
interpretRpcMsgpackProcessNvimEmbed conf =
  interpretProcessCerealNative def conf .
  interpretRpcMsgpackProcessSingle

interpretRpcMsgpackProcessNvimEmbedDef ::
  Members [Responses RequestId Response !! RpcError, Events res RpcMessage] r =>
  Members [Error ProcessError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcStack r
interpretRpcMsgpackProcessNvimEmbedDef =
  interpretProcessCerealNvimEmbed def .
  interpretRpcMsgpackProcessSingle

interpretRpcEmbed ::
  Members [Events er Event, Events res RpcMessage] r =>
  Members [Error ProcessError, Error Text, Log, Resource, Race, Async, Embed IO] r =>
  [RpcHandler (Rpc !! RpcError : r)] ->
  InterpreterFor (Rpc !! RpcError) r
interpretRpcEmbed handlers =
  interpretResponses .
  interpretRpcMsgpackProcessNvimEmbedDef .
  withRequestHandler (hoistRpcHandler (insertAt @2) <$> handlers) .
  insertAt @2 .
  raise

type EmbedStack =
  [
    Rpc !! RpcError,
    ChanConsumer Event,
    ChanConsumer RpcMessage,
    Error ProcessError,
    Log
  ]

runHostEmbedLog ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor EmbedStack r
runHostEmbedLog logLevel handlers =
  interpretLogStdoutLevelConc (Just logLevel) .
  mapError @ProcessError show .
  interpretEventsChan @RpcMessage .
  interpretEventsChan @Event .
  interpretRpcEmbed (hoistRpcHandler (raise2Under . raise3Under) <$> handlers) .
  raiseUnder .
  raise2Under

runHostEmbed ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor EmbedStack r
runHostEmbed =
  runHostEmbedLog Warn

embedNvimLog ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  Severity ->
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvimLog level handlers =
  runHostEmbedLog level handlers .
  resumeHoistError @_ @Rpc (show @Text)

embedNvim ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler (EmbedStack ++ r)] ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvim handlers =
  runHostEmbed handlers .
  resumeHoistError @_ @Rpc (show @Text)

embedNvim_ ::
  Members [Error Text, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor (Rpc : EmbedStack) r
embedNvim_ =
  embedNvim []
