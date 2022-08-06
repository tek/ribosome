module Ribosome.Host.RegisterHandlers where

import qualified Data.Text as Text
import Exon (exon)
import qualified Polysemy.Log as Log

import Ribosome.Host.Api.Autocmd (autocmd)
import Ribosome.Host.Api.Data (nvimCommand, nvimCreateUserCommand)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import Ribosome.Host.Data.Report (Report, resumeReport)
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError, rpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), rpcMethod)
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (CommandArgs (CommandArgs), CommandOptions (CommandOptions), RpcType, completionValue)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

registerFailed ::
  Member Log r =>
  RpcError ->
  Sem r ()
registerFailed e =
  Log.error [exon|Registering rpc handlers failed: #{rpcError e}|]

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
  RpcName ->
  Execution ->
  RpcType ->
  RpcCall ()
registerType i method (RpcName name) exec = \case
  RpcType.Function ->
    nvimCommand [exon|function! #{name}(...) range
return #{rpcCall i method exec (Just "a:000")}
endfunction|]
  RpcType.Command (CommandOptions options comp) (CommandArgs args) ->
    nvimCreateUserCommand name [exon|call #{rpcCall i method exec (Just argsText)}|] (options <> foldMap compOpt comp)
    where
      compOpt c =
        [("complete", toMsgpack (completionValue c))]
      argsText =
        [exon|[#{Text.intercalate ", " args}]|]
  RpcType.Autocmd events options ->
    void (autocmd events options [exon|call #{rpcCall i method exec Nothing}|])

registerHandler ::
  ChannelId ->
  RpcHandler r ->
  RpcCall ()
registerHandler i (RpcHandler tpe name exec _) =
  registerType i (rpcMethod tpe name) name exec tpe

registerHandlers ::
  Members [Rpc !! RpcError, Log] r =>
  [RpcHandler r] ->
  Sem (Stop Report : r) ()
registerHandlers defs = do
  i <- resumeReport Rpc.channelId
  Rpc.sync (foldMap (registerHandler i) defs) !! registerFailed
