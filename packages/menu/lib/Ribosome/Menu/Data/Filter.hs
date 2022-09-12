module Ribosome.Menu.Data.Filter where

import Ribosome.Menu.Class.FilterEnum (FilterEnum (cycle, describe))

data Filter =
  Substring
  |
  Fuzzy
  |
  Prefix
  deriving stock (Eq, Show, Ord)

instance FilterEnum Filter where

  cycle = \case
    Substring -> Fuzzy
    Fuzzy -> Prefix
    Prefix -> Substring

  describe = \case
    Substring -> "substring"
    Fuzzy -> "fuzzy"
    Prefix -> "prefix"
