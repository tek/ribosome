module Ribosome.Host.Data.CommandArgs where

data CommandArgs =
  CommandArgs {
    bang :: Maybe Bool,
    range :: Maybe (Int, Int),
    count :: Maybe Int,
    register :: Maybe String
  }
  deriving stock (Eq, Show, Generic)
