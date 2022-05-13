module Ribosome.Host.RegisterHandlers where

import qualified Data.Text as Text
import Exon (exon)
import qualified Polysemy.Log as Log
import Prelude hiding (group)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetApiInfo)
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import Ribosome.Host.Data.RpcError (RpcError (RpcError, unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), rpcMethod)
import qualified Ribosome.Host.Data.RpcType as AutocmdOptions
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (AutocmdEvent (AutocmdEvent), AutocmdOptions (AutocmdOptions), RpcType)
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
  RpcType ->
  Text ->
  RpcError ->
  Sem r ()
registerFailed tpe name (RpcError e) =
  Log.error [exon|Registering #{RpcType.methodPrefix tpe} '#{name}' failed: #{e}|]

rpcCall ::
  ChannelId ->
  RpcMethod ->
  Execution ->
  Text ->
  Text
rpcCall (ChannelId i) (RpcMethod method) exec args =
  [exon|#{trigger exec}(#{show i}, '#{method}', #{args})|]
  where
    trigger = \case
      Sync -> "rpcrequest"
      Async -> "rpcnotify"

registerType ::
  ChannelId ->
  RpcMethod ->
  Text ->
  Execution ->
  RpcType ->
  Text
registerType i method name exec = \case
  RpcType.Function ->
    [exon|function! #{name}(...) range
return #{rpcCall i method exec "a:000"}
endfunction|]
  RpcType.Command options args ->
    [exon|command! #{optionsText} #{name} call #{rpcCall i method exec argsText}|]
    where
      optionsText =
        Text.intercalate " " options
      argsText =
        [exon|[#{Text.intercalate ", " args}]|]
  (RpcType.Autocmd (AutocmdEvent event) (AutocmdOptions {..})) ->
    [exon|autocmd! #{fold group} #{event} #{fPattern} call #{rpcCall i method exec "[]"}|]

registerHandler ::
  Members [Rpc !! RpcError, Log] r =>
  ChannelId ->
  RpcHandler r ->
  Sem r ()
registerHandler i (RpcHandler tpe name exec _) =
  nvimCommand (registerType i (rpcMethod tpe name) name exec tpe) !! registerFailed tpe name

registerHandlers ::
  Member (Error Text) r =>
  Members [Rpc !! RpcError, Log] r =>
  [RpcHandler r] ->
  Sem r ()
registerHandlers defs = do
  i <- channelId
  traverse_ (registerHandler i) defs
