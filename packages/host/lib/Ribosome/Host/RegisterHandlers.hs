module Ribosome.Host.RegisterHandlers where

import qualified Data.Text as Text
import Exon (exon)
import qualified Polysemy.Log as Log
import Prelude hiding (group)

import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Api.Effect (nvimGetApiInfo)
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import qualified Ribosome.Host.Data.HandlerError as HandlerError
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError (RpcError, unRpcError))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), rpcMethod)
import qualified Ribosome.Host.Data.RpcType as AutocmdOptions
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (AutocmdEvent (AutocmdEvent), AutocmdOptions (AutocmdOptions), RpcType)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

channelId ::
  Members [Rpc !! RpcError, Stop HandlerError] r =>
  Sem r ChannelId
channelId =
  resumeHoist (HandlerError.simple . unRpcError) do
    nvimGetApiInfo >>= \case
      [fromMsgpack -> Right i, _] ->
        pure (ChannelId i)
      i ->
        stop (HandlerError.simple [exon|API info did not contain channel ID: #{show i}|])

registerFailed ::
  Member Log r =>
  RpcError ->
  Sem r ()
registerFailed (RpcError e) =
  Log.error [exon|Registering rpc handlers failed: #{e}|]

trigger :: Execution -> Text
trigger = \case
  Sync -> "rpcrequest"
  Async -> "rpcnotify"

rpcCall ::
  ChannelId ->
  RpcMethod ->
  Execution ->
  Maybe Text ->
  Text
rpcCall (ChannelId i) (RpcMethod method) exec args =
  [exon|call('#{trigger exec}', [#{show i}, '#{method}']#{foldMap appendArgs args})|]
  where
    appendArgs a =
      [exon| + #{a}|]

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
return #{rpcCall i method exec (Just "a:000")}
endfunction|]
  RpcType.Command options args ->
    [exon|command! #{optionsText} #{name} call #{rpcCall i method exec (Just argsText)}|]
    where
      optionsText =
        Text.intercalate " " options
      argsText =
        [exon|[#{Text.intercalate ", " args}]|]
  RpcType.Autocmd (AutocmdEvent event) AutocmdOptions {..} ->
    [exon|autocmd! #{fold group} #{event} #{fPattern} call #{rpcCall i method exec Nothing}|]

registerHandler ::
  ChannelId ->
  RpcHandler r ->
  RpcCall ()
registerHandler i (RpcHandler tpe name exec _) =
  nvimCommand (registerType i (rpcMethod tpe name) name exec tpe)

registerHandlers ::
  Members [Rpc !! RpcError, Log] r =>
  [RpcHandler r] ->
  Sem (Stop HandlerError : r) ()
registerHandlers defs = do
  i <- channelId
  Rpc.sync (foldMap (registerHandler i) defs) !! registerFailed
