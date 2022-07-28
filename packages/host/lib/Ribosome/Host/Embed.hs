module Ribosome.Host.Embed where

import Conc (ChanConsumer, ChanEvents, interpretEventsChan)
import Polysemy.Process (Process)
import qualified Polysemy.Process.Effect.Process as Process

import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Data.RpcMessage (RpcMessage)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.IOStack (BasicStack)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import Ribosome.Host.Interpreter.Host (testHost, withHost)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)

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

type EmbedExtra =
  [
    ChanEvents RpcMessage,
    ChanConsumer RpcMessage
  ]

type HostEmbedStack =
  RpcStack ++ EmbedExtra ++ RpcDeps

interpretEmbedExtra ::
  Members [Process RpcMessage o, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor EmbedExtra r
interpretEmbedExtra =
  interpretEventsChan @RpcMessage .
  publishRequests

interpretRpcDeps ::
  Members [Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorInfo .
  interpretProcessCerealNvimEmbed Nothing Nothing

interpretHostEmbed ::
  Members BasicStack r =>
  InterpretersFor HostEmbedStack r
interpretHostEmbed =
  interpretRpcDeps .
  interpretEmbedExtra .
  interpretRpcStack

withHostEmbed ::
  Members BasicStack r =>
  InterpreterFor (Handlers !! Report) (HostEmbedStack ++ r) ->
  InterpretersFor HostEmbedStack r
withHostEmbed handlers =
  interpretHostEmbed .
  handlers .
  withHost .
  insertAt @0

testHostEmbed ::
  Members BasicStack r =>
  InterpreterFor (Handlers !! Report) (HostEmbedStack ++ r) ->
  InterpretersFor (Rpc : HostEmbedStack) r
testHostEmbed handlers =
  interpretHostEmbed .
  handlers .
  testHost .
  insertAt @1

embedNvim ::
  Members BasicStack r =>
  [RpcHandler (HostEmbedStack ++ r)] ->
  InterpretersFor (Rpc : HostEmbedStack) r
embedNvim handlers =
  testHostEmbed (interpretHandlers handlers)

embedNvim_ ::
  Members BasicStack r =>
  InterpretersFor (Rpc : HostEmbedStack) r
embedNvim_ =
  testHostEmbed interpretHandlersNull
