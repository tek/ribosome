module Ribosome.Menu.Class.FilterEnum where

class (
    Show a,
    Ord a
  ) => FilterEnum a where
  cycle :: a -> a
  describe :: a -> Text
