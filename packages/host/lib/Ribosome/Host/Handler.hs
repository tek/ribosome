module Ribosome.Host.Handler where

import qualified Data.Text as Text

import Ribosome.Host.Data.Execution (Execution (Sync))
import qualified Ribosome.Host.Data.RpcHandler as RpcHandler
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler (RpcHandler))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (
  AutocmdEvent,
  AutocmdOptions,
  CommandArgs (CommandArgs),
  CommandCompletion (CompleteBuiltin, CompleteHandler),
  CommandOptions (CommandOptions),
  CompleteStyle (CompleteFiltered, CompleteUnfiltered),
  completionName,
  )
import Ribosome.Host.Handler.Codec (HandlerCodec (handlerCodec))
import Ribosome.Host.Handler.Command (CommandHandler (commandOptions), OptionStateZero)

rpcFunction ::
  ∀ r h .
  HandlerCodec h r =>
  Text ->
  Execution ->
  h ->
  RpcHandler r
rpcFunction name execution h =
  RpcHandler RpcType.Function name execution (handlerCodec h)

rpcCommand ::
  ∀ r h .
  HandlerCodec h r =>
  CommandHandler OptionStateZero h =>
  Text ->
  Execution ->
  h ->
  RpcHandler r
rpcCommand name execution h =
  RpcHandler (RpcType.Command (CommandOptions opts Nothing) (CommandArgs args)) name execution (handlerCodec h)
  where
    (opts, args) =
      commandOptions @OptionStateZero @h

complete ::
  CommandCompletion ->
  RpcHandler r ->
  RpcHandler r
complete c = \case
  RpcHandler (RpcType.Command (CommandOptions opts _) args) n e h ->
    RpcHandler (RpcType.Command (CommandOptions opts (Just c)) args) n e h
  h ->
    h

completeBuiltin ::
  Text ->
  RpcHandler r ->
  RpcHandler r
completeBuiltin f =
  complete (CompleteBuiltin f)

completeWith ::
  CompleteStyle ->
  (Text -> Text -> Int -> Handler r [Text]) ->
  RpcHandler r ->
  [RpcHandler r]
completeWith style f main@RpcHandler {name} =
  [
    complete (CompleteHandler style name) main,
    completer style
  ]
  where
    completer = \case
      CompleteFiltered ->
        rpcFunction cn Sync f
      CompleteUnfiltered ->
        rpcFunction cn Sync \ lead line pos -> Text.unlines <$> f lead line pos
    cn =
      completionName name

rpcAutocmd ::
  ∀ r h .
  HandlerCodec h r =>
  Text ->
  Execution ->
  AutocmdEvent ->
  AutocmdOptions ->
  h ->
  RpcHandler r
rpcAutocmd name execution event options h =
  RpcHandler (RpcType.Autocmd event options) name execution (handlerCodec h)

rpc ::
  ∀ r h .
  HandlerCodec h r =>
  CommandHandler OptionStateZero h =>
  Text ->
  Execution ->
  h ->
  [RpcHandler r]
rpc name execution h =
  [
    rpcFunction name execution h,
    rpcCommand name execution h
  ]
