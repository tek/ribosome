module Ribosome.Host.RegisterHandlers where

import Data.MessagePack (Object)
import qualified Data.Text as Text
import Exon (exon)
import qualified Polysemy.Log as Log
import Prelude hiding (group)

import Ribosome.Host.Api.Data (nvimCommand, nvimCreateAugroup, nvimCreateAutocmd, nvimCreateUserCommand)
import Ribosome.Host.Api.Effect (nvimGetApiInfo)
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Execution (Execution (Async, Sync))
import Ribosome.Host.Data.Report (Report, resumeReport)
import Ribosome.Host.Data.Request (RpcMethod (RpcMethod))
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcError (RpcError, rpcReport)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler), rpcMethod)
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Data.RpcType as AutocmdOptions
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (
  AutocmdEvents (AutocmdEvents),
  AutocmdGroup (AutocmdGroup),
  AutocmdOptions (AutocmdOptions),
  AutocmdPatterns (AutocmdPatterns),
  CommandArgs (CommandArgs),
  CommandOptions (CommandOptions),
  RpcType,
  completionValue,
  )
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

channelId ::
  Members [Rpc !! RpcError, Stop Report] r =>
  Sem r ChannelId
channelId =
  resumeReport do
    nvimGetApiInfo >>= \case
      [fromMsgpack -> Right i, _] ->
        pure (ChannelId i)
      i ->
        stop (fromString [exon|API info did not contain channel ID: #{show i}|])

registerFailed ::
  Member Log r =>
  RpcError ->
  Sem r ()
registerFailed e =
  Log.error [exon|Registering rpc handlers failed: #{rpcReport e}|]

wrapAugroup :: Text -> Maybe AutocmdGroup -> Text
wrapAugroup cmd = \case
  Just (AutocmdGroup g) ->
    [exon|augroup #{g}
#{cmd}
augroup END
|]
  Nothing ->
    cmd

withAugroup :: (Map Text Object -> RpcCall a) -> Maybe AutocmdGroup -> RpcCall ()
withAugroup f =
  void . \case
    Just (AutocmdGroup g) ->
      nvimCreateAugroup g [("clear", toMsgpack False)] *> f [("group", toMsgpack g)]
    Nothing ->
      f mempty

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
  RpcType.Autocmd (AutocmdEvents event) AutocmdOptions {pat = AutocmdPatterns pat, ..} ->
    flip withAugroup group \ grp -> nvimCreateAutocmd event (opts <> grp)
    where
      opts =
        msgpackMap ("pattern", pat) ("command", cmd) ("once", once) ("nested", nested)
      cmd =
        [exon|call #{rpcCall i method exec Nothing}|]

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
  i <- channelId
  Rpc.sync (foldMap (registerHandler i) defs) !! registerFailed
