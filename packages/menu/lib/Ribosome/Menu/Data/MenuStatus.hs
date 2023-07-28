module Ribosome.Menu.Data.MenuStatus where

data MenuStatus =
  MenuStatus {
    filter :: Text,
    middle :: Int -> Maybe Text,
    bottom :: Int -> [Text],
    itemCount :: Word,
    entryCount :: Word,
    cursor :: Word
  }
  deriving stock (Generic)
