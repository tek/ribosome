module Ribosome.Host.Embed where

import Data.Serialize (Serialize)
import Polysemy.Conc (interpretRace)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)
import Polysemy.Process (Process, ProcessOptions, withProcess)
import Polysemy.Process.Data.ProcessError (ProcessError)
import System.Process.Typed (ProcessConfig, proc)

import Ribosome.Host.Data.Request (RequestId)
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcHandler (RpcHandler, hoistRpcHandler)
import Ribosome.Host.Data.RpcError (RpcError)
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

interpretRpcMsgpackProcessSingle ::
  Member (Scoped () (Process RpcMessage o) !! ProcessError) r =>
  Members [Responses RequestId Response !! RpcError, Error ProcessError, Log, Async, Embed IO] r =>
  InterpretersFor [Rpc !! RpcError, Process RpcMessage o] r
interpretRpcMsgpackProcessSingle =
  resumeError .
  withProcess @() .
  raiseUnder .
  interpretRpcMsgpack

type RpcStack =
  [
    Rpc !! RpcError,
    Process RpcMessage (Either Text RpcMessage),
    Scoped () (Process RpcMessage (Either Text RpcMessage)) !! ProcessError
  ]

interpretRpcMsgpackProcessNvimEmbed ::
  Member (Responses RequestId Response !! RpcError) r =>
  Members [Error ProcessError, Log, Resource, Race, Async, Embed IO] r =>
  ProcessConfig () () () ->
  InterpretersFor RpcStack r
interpretRpcMsgpackProcessNvimEmbed conf =
  interpretProcessCerealNative def conf .
  interpretRpcMsgpackProcessSingle

interpretRpcMsgpackProcessNvimEmbedDef ::
  Member (Responses RequestId Response !! RpcError) r =>
  Members [Error ProcessError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcStack r
interpretRpcMsgpackProcessNvimEmbedDef =
  interpretProcessCerealNvimEmbed def .
  interpretRpcMsgpackProcessSingle

interpretRpcEmbed ::
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
    Error ProcessError,
    Log,
    Race,
    Async
  ]

runNvimPluginEmbedLog ::
  Members [Error Text, Resource, Embed IO, Final IO] r =>
  Severity ->
  [RpcHandler (Rpc !! RpcError : r)] ->
  InterpretersFor EmbedStack r
runNvimPluginEmbedLog logLevel handlers =
  asyncToIOFinal .
  interpretRace .
  interpretLogStdoutLevelConc (Just logLevel) .
  mapError @ProcessError show .
  interpretRpcEmbed (hoistRpcHandler (insertAt @2) <$> handlers) .
  insertAt @1

runNvimPluginEmbed ::
  Members [Error Text, Resource, Embed IO, Final IO] r =>
  [RpcHandler (Rpc !! RpcError : r)] ->
  InterpretersFor EmbedStack r
runNvimPluginEmbed =
  runNvimPluginEmbedLog Warn
