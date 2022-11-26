module Ribosome.Menu.Data.MenuStatus where

data MenuStatus =
  MenuStatus {
    filter :: Text,
    middle :: Int -> Maybe Text,
    bottom :: Int -> [Text],
    itemCount :: Int,
    entryCount :: Int,
    cursor :: Int
  }
  deriving stock (Generic)
