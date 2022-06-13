module Ribosome.Host.Data.RpcType where

import Exon (exon)

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

data CompleteStyle =
  CompleteFiltered
  |
  CompleteUnfiltered
  deriving stock (Eq, Show)

data CommandCompletion =
  CompleteBuiltin Text
  |
  CompleteHandler CompleteStyle Text
  deriving stock (Eq, Show)

completionName ::
  Text ->
  Text
completionName n =
  [exon|Complete_#{n}|]

completionOption :: CommandCompletion -> Text
completionOption = \case
  CompleteBuiltin completer ->
    [exon|-complete=#{completer}|]
  CompleteHandler CompleteFiltered func ->
    [exon|-complete=customlist,#{completionName func}|]
  CompleteHandler CompleteUnfiltered func ->
    [exon|-complete=custom,#{completionName func}|]

data CommandOptions =
  CommandOptions {
    basic :: [Text],
    completion :: Maybe CommandCompletion
  }
  deriving stock (Show)

newtype CommandArgs =
  CommandArgs { unCommandArgs :: [Text] }
  deriving stock (Eq, Show)

data RpcType =
  Function
  |
  Command CommandOptions CommandArgs
  |
  Autocmd AutocmdEvent AutocmdOptions
  deriving stock (Show, Generic)

methodPrefix :: RpcType -> Text
methodPrefix = \case
  Function -> "function"
  Command _ _ -> "command"
  Autocmd _ _ -> "autocmd"
