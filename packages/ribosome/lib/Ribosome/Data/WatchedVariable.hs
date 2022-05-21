module Ribosome.Data.WatchedVariable where

newtype WatchedVariable =
  WatchedVariable { unWatchedVariable :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
