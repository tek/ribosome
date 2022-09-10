module Ribosome.Menu.Data.Filter where

data Filter =
  Substring
  |
  Fuzzy Bool
  deriving stock (Eq, Show, Ord)

fuzzyMono :: Filter
fuzzyMono =
  Fuzzy True
