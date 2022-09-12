module Ribosome.Menu.Interpreter.MenuFilter where

import qualified Data.IntMap.Strict as IntMap

import Ribosome.Menu.Data.Filter (Filter (Fuzzy, Prefix, Substring))
import Ribosome.Menu.Data.MenuItems (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (MenuFilter (Initial, Match, Refine))
import Ribosome.Menu.Filters (
  filterFuzzy,
  initialPrefix,
  initialSubstring,
  matchFuzzy,
  matchPrefix,
  matchSubstring,
  refineFuzzy,
  refinePrefix,
  refineSubstring,
  )

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
    Match Fuzzy query index item ->
      pure (matchFuzzy False query index item)
    Initial Fuzzy (MenuQuery (toString -> query)) items ->
      embed (filterFuzzy True query (IntMap.toList items))
    Refine Fuzzy (MenuQuery (toString -> query)) items ->
      embed (refineFuzzy query (concatMap toList (IntMap.elems items)))
    Match Prefix query index item ->
      pure (matchPrefix query index item)
    Initial Prefix query items ->
      pure (initialPrefix query items)
    Refine Prefix query items ->
      pure (refinePrefix query items)
