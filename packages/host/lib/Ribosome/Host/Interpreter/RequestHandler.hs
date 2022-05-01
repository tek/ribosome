module Ribosome.Host.Interpreter.RequestHandler where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Exon (exon)
import Polysemy.Conc (withAsync_)
import Polysemy.Process (Process)

import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError))
import Ribosome.Host.Data.Request (Request (Request), RequestId, RpcMethod (RpcMethod))
import Ribosome.Host.Data.RpcDef (RpcDef (RpcDef), RpcHandler)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.RequestHandler (RequestHandler (Handle))
import Ribosome.Host.Effect.Responses (Responses)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handlers (registerHandlers)
import Ribosome.Host.Listener (listener)

handlersByName ::
  [RpcDef r] ->
  Map RpcMethod (RpcHandler r)
handlersByName =
  Map.fromList . fmap \ (RpcDef _ name _ handler) -> (RpcMethod name, handler)

invalidMethod ::
  Member (Stop HandlerError) r =>
  RpcMethod ->
  Sem r a
invalidMethod (RpcMethod name) =
  stop (HandlerError [exon|Invalid method: #{name}|])

executeRequest ::
  [Object] ->
  RpcHandler r ->
  Sem (Stop HandlerError : r) Object
executeRequest args handle =
  stopOnError (raiseUnder (handle args))

interpretRequestHandler ::
  Member (Rpc !! RpcError) r =>
  [RpcDef r] ->
  InterpreterFor (RequestHandler !! HandlerError) r
interpretRequestHandler defs =
  interpretResumable \case
    Handle (Request name args) ->
      maybe (invalidMethod name) (executeRequest args) (Map.lookup name handlers)
  where
    handlers =
      handlersByName defs

withRequestHandler ::
  Members [Process Object (Either Text Object), Rpc !! RpcError, Log, Error Text] r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Resource, Race, Async] r =>
  [RpcDef r] ->
  InterpreterFor (RequestHandler !! HandlerError) r
withRequestHandler defs sem = do
  interpretRequestHandler defs do
    withAsync_ listener do
      raise (registerHandlers defs)
      sem

runRequestHandler ::
  Members [Process Object (Either Text Object), Rpc !! RpcError, Log, Error Text] r =>
  Members [Responses RequestId (Either RpcError Object) !! RpcError, Resource, Race, Async] r =>
  [RpcDef r] ->
  Sem r ()
runRequestHandler defs = do
  interpretRequestHandler defs do
    withAsync_ (raise (registerHandlers defs)) do
      listener
