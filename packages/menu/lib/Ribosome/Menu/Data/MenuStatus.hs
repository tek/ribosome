module Ribosome.Menu.Data.MenuStatus where

data MenuStatus =
  MenuStatus {
    filter :: Text,
    extra :: Maybe Text,
    itemCount :: Int,
    entryCount :: Int,
    cursor :: Int
  }
  deriving stock (Eq, Show, Generic)
