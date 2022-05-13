module Ribosome.Host.Data.RpcType where

newtype AutocmdEvent =
  AutocmdEvent { unAutocmdEvent :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data AutocmdOptions =
  AutocmdOptions {
    fPattern :: Text,
    nested :: Bool,
    once :: Bool,
    group :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance Default AutocmdOptions where
  def =
    AutocmdOptions "*" False False Nothing

data RpcType =
  Function
  |
  Command [Text] [Text]
  |
  Autocmd AutocmdEvent AutocmdOptions
  deriving stock (Show, Generic)

camel :: RpcType -> Text
camel = \case
  Function -> "Function"
  Command _ _ -> "Command"
  Autocmd _ _ -> "Autocmd"

methodPrefix :: RpcType -> Text
methodPrefix = \case
  Function -> "function"
  Command _ _ -> "command"
  Autocmd _ _ -> "autocmd"
