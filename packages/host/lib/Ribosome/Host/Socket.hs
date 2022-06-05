module Ribosome.Host.Socket where

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.NvimSocket (NvimSocket)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.IOStack (BasicStack)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import Ribosome.Host.Interpreter.Host (testHost, withHost)
import Ribosome.Host.Interpreter.Process.Socket (interpretProcessCerealSocket)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)

type HostSocketStack =
  RpcStack ++ RpcDeps

interpretRpcDeps ::
  Members [Reader NvimSocket, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorInfo .
  interpretProcessCerealSocket def .
  resumeHoistError (BootError . show @Text) .
  raiseUnder

interpretHostSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor HostSocketStack r
interpretHostSocket =
  interpretRpcDeps .
  interpretRpcStack

withHostSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  InterpreterFor (Handlers !! HandlerError) (HostSocketStack ++ r) ->
  InterpretersFor HostSocketStack r
withHostSocket handlers =
  interpretHostSocket .
  handlers .
  withHost .
  insertAt @0

testHostSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  InterpreterFor (Handlers !! HandlerError) (HostSocketStack ++ r) ->
  InterpretersFor (Rpc : HostSocketStack) r
testHostSocket handlers =
  interpretHostSocket .
  handlers .
  testHost .
  insertAt @1

runHostSocket ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  [RpcHandler (HostSocketStack ++ r)] ->
  InterpretersFor (Rpc : HostSocketStack) r
runHostSocket handlers =
  testHostSocket (interpretHandlers handlers)

runHostSocket_ ::
  Members BasicStack r =>
  Member (Reader NvimSocket) r =>
  InterpretersFor (Rpc : HostSocketStack) r
runHostSocket_ =
  testHostSocket interpretHandlersNull
