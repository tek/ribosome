module Ribosome.Host.Interpreter.Host where

import Conc (withAsync_)
import Data.MessagePack (Object (ObjectNil))
import Exon (exon)
import Log (Severity (Error, Warn), dataLog)
import Polysemy.Process (Process)
import System.IO.Error (IOError)

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.Event (Event (Event), EventName (EventName))
import Ribosome.Host.Data.Report (LogReport (LogReport), Report (Report), severity)
import Ribosome.Host.Data.Request (Request (Request), RequestId, RpcMethod (RpcMethod))
import qualified Ribosome.Host.Data.Response as Response
import Ribosome.Host.Data.Response (Response)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import qualified Ribosome.Host.Effect.Handlers as Handlers
import Ribosome.Host.Effect.Handlers (Handlers)
import qualified Ribosome.Host.Effect.Host as Host
import Ribosome.Host.Effect.Host (Host)
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Listener (listener)

invalidMethod ::
  Request ->
  Response
invalidMethod (Request (RpcMethod name) _) =
  Response.Error [exon|Invalid method for request: #{name}|]

publishEvent ::
  Member (Events Event) r =>
  Request ->
  Sem r ()
publishEvent (Request (RpcMethod name) args) =
  publish (Event (EventName name) args)

handlerIOError ::
  Members [Error Report, Final IO] r =>
  Sem r a ->
  Sem r a
handlerIOError =
  fromExceptionSemVia \ (e :: IOError) ->
    Report "Internal error" ["Handler exception", show e] Error

handlerReport ::
  Member (DataLog LogReport) r =>
  Bool ->
  RpcMethod ->
  Report ->
  Sem r ()
handlerReport notification (RpcMethod method) r =
  dataLog (LogReport r (notification || r.severity < Error) (r.severity >= Warn) (fromText method))

handle ::
  Members [Handlers !! Report, Rpc !! RpcError, DataLog LogReport, Log, Final IO] r =>
  Bool ->
  Request ->
  Sem r (Maybe Response)
handle notification (Request method args) =
  errorToIOFinal (handlerIOError (resuming throw (Handlers.run method args))) >>= \case
    Right Nothing ->
      pure Nothing
    Right (Just a) ->
      pure (Just (Response.Success a))
    Left r@(Report e _ severity) -> do
      handlerReport notification method r
      let
        response =
          if severity < Error then Response.Success ObjectNil else Response.Error e
      pure (Just response)

interpretHost ::
  Members [Handlers !! Report, Rpc !! RpcError, DataLog LogReport, Events Event, Log, Final IO] r =>
  InterpreterFor Host r
interpretHost =
  interpret \case
    Host.Request req ->
      fromMaybe (invalidMethod req) <$> handle False req
    Host.Notification req -> do
      res <- handle True req
      when (isNothing res) (publishEvent req)

register ::
  Members [Handlers !! Report, Error BootError] r =>
  Sem r ()
register =
  Handlers.register !! \ e -> throw (BootError [exon|Registering handlers: #{show e}|])

type HostDeps er =
  [
    Handlers !! Report,
    Process RpcMessage (Either Text RpcMessage),
    Rpc !! RpcError,
    DataLog LogReport,
    Events Event,
    Responses RequestId Response !! RpcError,
    Log,
    Error BootError,
    Resource,
    Mask,
    Race,
    Async,
    Embed IO,
    Final IO
  ]

withHost ::
  Members (HostDeps er) r =>
  InterpreterFor Host r
withHost sem =
  interpretHost do
    withAsync_ listener do
      register
      sem

testHost ::
  Members (HostDeps er) r =>
  InterpretersFor [Rpc, Host] r
testHost =
  withHost .
  resumeBootError @Rpc

runHost ::
  Members (HostDeps er) r =>
  Sem r ()
runHost =
  interpretHost do
    withAsync_ register do
      listener
