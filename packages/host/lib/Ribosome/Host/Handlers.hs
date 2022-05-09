module Ribosome.Host.Handlers where

import Data.MessagePack (Object (ObjectMap))
import Exon (exon)
import qualified Polysemy.Log as Log

import Ribosome.Host.Api.Effect (nvimCallFunction, nvimGetApiInfo)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Class.Msgpack.Map (MsgpackMap (msgpackMap))
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), rpcMethod)
import Ribosome.Host.Data.RpcError (RpcError (RpcError, unRpcError))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (AutocmdEvent (AutocmdEvent), AutocmdOpts (AutocmdOpts))
import Ribosome.Host.Effect.Rpc (Rpc)

channelId ::
  Member (Error Text) r =>
  Member (Rpc !! RpcError) r =>
  Sem r ChannelId
channelId =
  resumeHoistError unRpcError do
    nvimGetApiInfo >>= \case
      [fromMsgpack -> Right i, _] ->
        pure (ChannelId i)
      i ->
        throw [exon|API info did not contain channel ID: #{show i}|]

registerFailed ::
  Member Log r =>
  RpcHandler r ->
  RpcError ->
  Sem r ()
registerFailed (RpcHandler tpe name _ _) (RpcError e) =
  Log.error [exon|Registering #{RpcType.methodPrefix tpe} '#{name}' failed: #{e}|]

autocmdOpts :: AutocmdOpts -> Object
autocmdOpts (AutocmdOpts pat nested once grp) =
  msgpackMap ("pattern", pat) ("nested", nested) ("once", once) ("group", grp)

define :: RpcHandler r -> Text
define (RpcHandler tpe _ _ _) =
  [exon|remote#define##{RpcType.camel tpe}OnChannel|]

registerArgs :: RpcHandler r -> [Object]
registerArgs = \case
  RpcHandler RpcType.Function name exec _ ->
    msgpackArray exec name (ObjectMap mempty)
  RpcHandler RpcType.Command name exec _ ->
    msgpackArray exec name (ObjectMap mempty)
  RpcHandler (RpcType.Autocmd (AutocmdEvent event) opts) _ exec _ ->
    msgpackArray exec event (autocmdOpts opts)

registerHandler ::
  Members [Rpc !! RpcError, Log] r =>
  ChannelId ->
  RpcHandler r ->
  Sem r ()
registerHandler (ChannelId i) rpcDef =
  nvimCallFunction (define rpcDef) (msgpackArray i method <> registerArgs rpcDef) !! registerFailed rpcDef
  where
    method =
      rpcMethod rpcDef

registerHandlers ::
  Member (Error Text) r =>
  Members [Rpc !! RpcError, Log] r =>
  [RpcHandler r] ->
  Sem r ()
registerHandlers defs = do
  i <- channelId
  traverse_ (registerHandler i) defs
