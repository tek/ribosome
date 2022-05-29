module Ribosome.Host.Embed where

import Data.Serialize (Serialize)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import qualified Polysemy.Process as Process
import Polysemy.Process (Process, ProcessOptions, withProcess)
import Polysemy.Process.Data.ProcessError (ProcessError)
import System.Process.Typed (ProcessConfig, proc)

import Ribosome.Host.Config (interpretLogConfig)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (HostConfig, LogConfig)
import Ribosome.Host.Data.HostError (HostError)
import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Log (FileLog, StderrLog)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Effect.UserError (UserError)
import Ribosome.Host.Interpreter.Errors (interpretErrors)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull)
import Ribosome.Host.Interpreter.Host (testHost, withHost)
import Ribosome.Host.Interpreter.Log (interpretDataLogRpc, interpretLogStderrFile, interpretLogs)
import Ribosome.Host.Interpreter.Process (interpretProcessCerealNative)
import Ribosome.Host.Interpreter.Responses (interpretResponses)
import Ribosome.Host.Interpreter.Rpc (interpretRpc)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)

basicCliArgs :: [String]
basicCliArgs =
  ["--embed", "-n", "-u", "NONE", "-i", "NONE", "--clean", "--headless"]

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

type RpcStack =
  [
    Rpc !! RpcError,
    Process RpcMessage (Either Text RpcMessage),
    Scoped () (Process RpcMessage (Either Text RpcMessage)) !! ProcessError
  ]

interpretRpcSingle ::
  Member (Events res RpcMessage) r =>
  Maybe ProcessOptions ->
  Maybe (ProcessConfig () () ()) ->
  Members [Responses RequestId Response !! RpcError, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcStack r
interpretRpcSingle options conf =
  interpretProcessCerealNative (fromMaybe def options) (fromMaybe embeddedProcessConfig conf) .
  resumeHoistError (BootError . show) .
  withProcess @() .
  raiseUnder .
  publishRequests .
  interpretRpc

type CoreStack =
  [
    DataLog HostError,
    Rpc !! RpcError,
    Process RpcMessage (Either Text RpcMessage),
    Scoped () (Process RpcMessage (Either Text RpcMessage)) !! ProcessError,
    Responses RequestId Response !! RpcError,
    ChanEvents Event,
    ChanConsumer Event,
    ChanEvents RpcMessage,
    ChanConsumer RpcMessage,
    Errors
  ]

type CoreDeps =
  [
    Log,
    StderrLog,
    FileLog,
    Reader LogConfig,
    Reader HostConfig
  ]

type EmbedStack =
  CoreStack ++ UserError : CoreDeps

interpretHostEmbedCore ::
  Members CoreDeps r =>
  Members [UserError, ChronosTime, Error BootError, Resource, Race, Async, Embed IO] r =>
  Maybe ProcessOptions ->
  Maybe (ProcessConfig () () ()) ->
  InterpretersFor CoreStack r
interpretHostEmbedCore options conf =
  interpretErrors .
  interpretEventsChan @RpcMessage .
  interpretEventsChan @Event .
  interpretResponses .
  interpretRpcSingle options conf .
  interpretDataLogRpc

interpretCoreDeps ::
  Members [ChronosTime, Error BootError, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  InterpretersFor CoreDeps r
interpretCoreDeps conf =
  runReader conf .
  interpretLogConfig .
  interpretLogs .
  interpretLogStderrFile

interpretHostEmbed ::
  Members [ChronosTime, Error BootError, Resource, Race, Async, Embed IO] r =>
  HostConfig ->
  InterpretersFor EmbedStack r
interpretHostEmbed conf =
  interpretCoreDeps conf .
  interpretUserErrorInfo .
  interpretHostEmbedCore Nothing Nothing

withHostEmbed ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  HostConfig ->
  InterpreterFor (Handlers !! HandlerError) (EmbedStack ++ r) ->
  InterpretersFor EmbedStack r
withHostEmbed conf handlers =
  interpretHostEmbed conf .
  handlers .
  withHost .
  insertAt @0

embedNvimConf ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  HostConfig ->
  InterpreterFor (Handlers !! HandlerError) (EmbedStack ++ r) ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvimConf conf handlers =
  interpretHostEmbed conf .
  handlers .
  testHost .
  insertAt @1

embedNvim ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpreterFor (Handlers !! HandlerError) (EmbedStack ++ r) ->
  InterpretersFor (Rpc : EmbedStack) r
embedNvim =
  embedNvimConf def

embedNvim_ ::
  Members [Error BootError, ChronosTime, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor (Rpc : EmbedStack) r
embedNvim_ =
  embedNvim interpretHandlersNull
