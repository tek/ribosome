module Ribosome.Menu.Data.Filter where

import Ribosome.Menu.Class.FilterEnum (FilterEnum (cycle))

data Filter =
  Substring
  |
  Fuzzy Bool
  deriving stock (Eq, Show, Ord)

instance FilterEnum Filter where
  cycle = \case
    Substring -> Fuzzy True
    Fuzzy True -> Fuzzy False
    Fuzzy False -> Substring

fuzzyMono :: Filter
fuzzyMono =
  Fuzzy True
