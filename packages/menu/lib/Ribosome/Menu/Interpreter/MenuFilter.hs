module Ribosome.Menu.Interpreter.MenuFilter where

import qualified Data.IntMap.Strict as IntMap

import Ribosome.Menu.Data.Filter (Filter (Fuzzy, Substring))
import Ribosome.Menu.Data.MenuItems (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (MenuFilter (Initial, Match, Refine))
import Ribosome.Menu.Filters (filterFuzzy, initialSubstring, matchFuzzy, matchSubstring, refineFuzzy, refineSubstring)

defaultFilter ::
  Member (Embed IO) r =>
  InterpreterFor (MenuFilter Filter) r
defaultFilter =
  interpret \case
    Match Substring query index item ->
      pure (matchSubstring query index item)
    Initial Substring query items ->
      pure (initialSubstring query items)
    Refine Substring query items ->
      pure (refineSubstring query items)
    Match (Fuzzy _) query index item ->
      pure (matchFuzzy False query index item)
    Initial (Fuzzy monotonic) (MenuQuery (toString -> query)) items ->
      embed (filterFuzzy monotonic query (IntMap.toList items))
    Refine (Fuzzy _) (MenuQuery (toString -> query)) items ->
      embed (refineFuzzy query (concatMap toList (IntMap.elems items)))
