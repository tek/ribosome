module Ribosome.Host.Data.RpcType where

newtype AutocmdEvent =
  AutocmdEvent { unAutocmdEvent :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

data AutocmdOpts =
  AutocmdOpts {
    fPattern :: Text,
    nested :: Bool,
    once :: Bool,
    group :: Maybe Text
  }
  deriving stock (Eq, Show)

data RpcType =
  Function
  |
  Command
  |
  Autocmd AutocmdEvent AutocmdOpts
  deriving stock (Show)

camel :: RpcType -> Text
camel = \case
  Function -> "Function"
  Command -> "Command"
  Autocmd _ _ -> "Autocmd"

methodPrefix :: RpcType -> Text
methodPrefix = \case
  Function -> "function"
  Command -> "command"
  Autocmd _ _ -> "autocmd"
