module Ribosome.Host.Handlers where

import Data.MessagePack (Object (ObjectMap))
import Exon (exon)
import qualified Polysemy.Log as Log

import Ribosome.Host.Api.Effect (nvimCallFunction, nvimGetApiInfo)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Class.Msgpack.Map (MsgpackMap (msgpackMap))
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.RpcDef (RpcDef (RpcDef))
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
  Text ->
  Text ->
  RpcError ->
  Sem r ()
registerFailed tpe name (RpcError e) =
  Log.error [exon|Registering #{tpe} '#{name}' failed: #{e}|]

autocmdOpts :: AutocmdOpts -> Object
autocmdOpts (AutocmdOpts pat nested once grp) =
  msgpackMap ("pattern", pat) ("nested", nested) ("once", once) ("group", grp)

registerHandler ::
  Members [Rpc !! RpcError, Log] r =>
  ChannelId ->
  RpcDef r ->
  Sem r ()
registerHandler (ChannelId i) = \case
  RpcDef RpcType.Function name exec _ ->
    reg "Function" name (msgpackArray exec name (ObjectMap mempty))
  RpcDef RpcType.Command name exec _ ->
    reg "Command" name (msgpackArray exec name (ObjectMap mempty))
  RpcDef (RpcType.Autocmd (AutocmdEvent event) opts) name exec _ ->
    reg "Autocmd" name (msgpackArray exec event (autocmdOpts opts))
  where
    reg tpe name args =
      nvimCallFunction [exon|remote#define##{tpe}OnChannel|] (msgpackArray i name <> args) !!
      registerFailed tpe name

registerHandlers ::
  Member (Error Text) r =>
  Members [Rpc !! RpcError, Log] r =>
  [RpcDef r] ->
  Sem r ()
registerHandlers defs = do
  i <- channelId
  traverse_ (registerHandler i) defs
