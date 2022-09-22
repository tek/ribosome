module Ribosome.Menu.Data.Filter where

import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (MenuMode (cycleFilter, renderExtra, renderFilter))
import Ribosome.Menu.Data.FilterMode (FilterMode (FilterMode))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem

data Filter =
  Substring
  |
  Fuzzy
  |
  Prefix
  |
  Regex
  deriving stock (Eq, Show, Ord)

instance Default Filter where
  def =
    Fuzzy

instance MenuMode i Filter where

  type Filter Filter =
    FilterMode Filter

  cycleFilter = \case
    Substring -> Fuzzy
    Fuzzy -> Prefix
    Prefix -> Regex
    Regex -> Substring

  renderFilter = \case
    Substring -> "substring"
    Fuzzy -> "fuzzy"
    Prefix -> "prefix"
    Regex -> "regex"

  renderExtra =
    const Nothing

  filterMode f =
    FilterMode f (Just . MenuItem.text)
