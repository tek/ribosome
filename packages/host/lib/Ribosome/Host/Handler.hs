module Ribosome.Host.Handler where

import qualified Data.Text as Text

import Ribosome.Host.Data.Execution (Execution (Sync))
import qualified Ribosome.Host.Data.RpcHandler as RpcHandler
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler (RpcHandler))
import Ribosome.Host.Data.RpcName (RpcName)
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

-- |Create an 'RpcHandler' that is triggered by a Neovim function of the specified name.
--
-- The handler can take arbitrary parameters, as long as they are instances of 'Ribosome.MsgpackDecode' (or more
-- specifically, 'Ribosome.Host.Handler.Codec.HandlerArg'), just like the return type.
--
-- When invoking the function from Neovim, a value must be passed for each of the handler function's parameters, except
-- for some special cases, like a number of successive 'Maybe' parameters at the tail of the parameter list.
--
-- The function is converted to use messagepack types by the class 'HandlerCodec'.
--
-- For easier type inference, it is advisable to use @'Handler' r a@ for the return type of the handler instead of using
-- @'Member' ('Stop' 'Ribosome.HandlerError') r@.
--
-- Example:
--
-- > import Ribosome
-- >
-- > ping :: Int -> Handler r Int
-- > ping 0 = basicHandlerError "Invalid ping number!" ["This is written to the log"]
-- > ping i = pure i
-- >
-- > rpcFunction "Ping" Sync ping
rpcFunction ::
  ∀ r h .
  HandlerCodec h r =>
  -- |Name of the Neovim function that will be created.
  RpcName ->
  -- |Execute sync or async.
  Execution ->
  -- |The handler function.
  h ->
  RpcHandler r
rpcFunction name execution h =
  RpcHandler RpcType.Function name execution (handlerCodec h)

-- |Create an 'RpcHandler' that is triggered by a Neovim command of the specified name.
--
-- The handler can take arbitrary parameters, as long as they are instances of 'Ribosome.MsgpackDecode' (or more
-- specifically, 'Ribosome.Host.Handler.Codec.HandlerArg'), just like the return type.
-- The function is converted to use messagepack types by the class 'HandlerCodec'.
--
-- Commands have an (open) family of special parameter types that will be translated into command options, like
-- 'Ribosome.Range' for the line range specified to the command.
-- See "Ribosome#command-params".
--
-- For easier type inference, it is advisable to use @'Handler' r a@ for the return type of the handler instead of using
-- @'Member' ('Stop' 'HandlerError') r@.
rpcCommand ::
  ∀ r h .
  HandlerCodec h r =>
  CommandHandler OptionStateZero h =>
  -- |Name of the Neovim function that will be created.
  RpcName ->
  -- |Execute sync or async.
  Execution ->
  -- |The handler function.
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

completeCustom ::
  RpcName ->
  (Text -> Text -> Int -> Handler r [Text]) ->
  CompleteStyle ->
  RpcHandler r
completeCustom name f = \case
  CompleteFiltered ->
    rpcFunction cn Sync f
  CompleteUnfiltered ->
    rpcFunction cn Sync \ lead line pos -> Text.unlines <$> f lead line pos
  where
    cn =
      completionName name

completeWith ::
  CompleteStyle ->
  (Text -> Text -> Int -> Handler r [Text]) ->
  RpcHandler r ->
  [RpcHandler r]
completeWith style f main@RpcHandler {rpcName} =
  [
    complete (CompleteHandler style rpcName) main,
    completeCustom rpcName f style
  ]

-- |Create an 'RpcHandler' that is triggered by a Neovim autocommand for the specified event.
-- For a user autocommand, specify @User@ for the event and the event name for the file pattern in 'AutocmdOptions'.
--
-- For easier type inference, it is advisable to use @'Handler' r a@ for the return type of the handler instead of using
-- @'Member' ('Stop' 'HandlerError') r@.
rpcAutocmd ::
  ∀ r h .
  HandlerCodec h r =>
  RpcName ->
  -- |Execute sync or async. While autocommands can not interact with return values, this is still useful to keep Neovim
  -- from continuing execution while the handler is active, which is particularly important for @VimLeave@.
  Execution ->
  -- |The Neovim event identifier, like @BufWritePre@ or @User@.
  AutocmdEvent ->
  -- |Various Neovim options like the file pattern.
  AutocmdOptions ->
  -- |The handler function.
  h ->
  RpcHandler r
rpcAutocmd name execution event options h =
  RpcHandler (RpcType.Autocmd event options) name execution (handlerCodec h)

-- |Convenience function for creating a handler that is triggered by both a function and a command of the same name.
-- See 'rpcFunction' and 'rpcCommand'.
rpc ::
  ∀ r h .
  HandlerCodec h r =>
  CommandHandler OptionStateZero h =>
  RpcName ->
  Execution ->
  h ->
  [RpcHandler r]
rpc name execution h =
  [
    rpcFunction name execution h,
    rpcCommand name execution h
  ]
