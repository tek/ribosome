module Ribosome.Menu.Interpreter.MenuFilter where

import qualified Data.IntMap.Strict as IntMap

import Ribosome.Menu.Data.MenuData (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (MenuFilter (Initial, Match, Refine))
import Ribosome.Menu.Filters (filterFuzzy, initialSubstring, matchFuzzy, matchSubstring, refineFuzzy, refineSubstring)

interpretMenuFilterSubstring :: InterpreterFor MenuFilter r
interpretMenuFilterSubstring =
  interpret \case
    Match query index item ->
      pure (matchSubstring query index item)
    Initial query items ->
      pure (initialSubstring query items)
    Refine query items ->
      pure (refineSubstring query items)

interpretMenuFilterFuzzyWith ::
  Member (Embed IO) r =>
  Bool ->
  InterpreterFor MenuFilter r
interpretMenuFilterFuzzyWith monotonic =
  interpret \case
    Match query index item ->
      pure (matchFuzzy False query index item)
    Initial (MenuQuery (toString -> query)) items ->
      embed (filterFuzzy monotonic query (IntMap.toList items))
    Refine (MenuQuery (toString -> query)) items ->
      embed (refineFuzzy query (concatMap toList (IntMap.elems items)))

interpretMenuFilterFuzzy ::
  Member (Embed IO) r =>
  InterpreterFor MenuFilter r
interpretMenuFilterFuzzy =
  interpretMenuFilterFuzzyWith False

interpretMenuFilterFuzzyMonotonic ::
  Member (Embed IO) r =>
  InterpreterFor MenuFilter r
interpretMenuFilterFuzzyMonotonic =
  interpretMenuFilterFuzzyWith True
