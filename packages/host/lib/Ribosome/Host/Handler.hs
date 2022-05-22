module Ribosome.Host.Handler where

import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler))
import qualified Ribosome.Host.Data.RpcType as RpcType
import Ribosome.Host.Data.RpcType (AutocmdEvent, AutocmdOptions)
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
  RpcHandler (RpcType.Command opts args) name execution (handlerCodec h)
  where
    (opts, args) =
      commandOptions @OptionStateZero @h

rpcAutocmd ::
  ∀ r h .
  HandlerCodec h r =>
  Text ->
  AutocmdEvent ->
  AutocmdOptions ->
  h ->
  RpcHandler r
rpcAutocmd name event options h =
  RpcHandler (RpcType.Autocmd event options) name Async (handlerCodec h)
