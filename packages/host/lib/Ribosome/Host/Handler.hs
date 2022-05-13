module Ribosome.Host.Handler where

import Ribosome.Host.Data.Execution (Execution)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (AutocmdEvent (AutocmdEvent), AutocmdOptions)
import Ribosome.Host.Handler.Codec (HandlerCodec (handlerCodec))
import Ribosome.Host.Handler.Command (CommandHandler (commandOptions), OptionStateZero)

rpcFunction :: HandlerCodec h r => Text -> Execution -> h -> RpcHandler r
rpcFunction name execution h =
  RpcHandler RpcType.Function name execution (handlerCodec h)

rpcCommand ::
  ∀ h r .
  HandlerCodec h r =>
  CommandHandler OptionStateZero h =>
  Text ->
  Execution ->
  h ->
  RpcHandler r
rpcCommand name execution h =
  RpcHandler (RpcType.Command opts args) name execution (handlerCodec h)
  where
    (opts, args) =
      commandOptions @OptionStateZero @h

rpcAutocmd ::
  HandlerCodec h r =>
  Text ->
  Execution ->
  AutocmdOptions ->
  h ->
  RpcHandler r
rpcAutocmd name execution options h =
  RpcHandler (RpcType.Autocmd (AutocmdEvent name) options) name execution (handlerCodec h)
