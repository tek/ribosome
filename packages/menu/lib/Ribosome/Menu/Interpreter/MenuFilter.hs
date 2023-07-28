module Ribosome.Menu.Interpreter.MenuFilter where

import Text.Regex.PCRE.Light.Char8 (caseless, compileM, utf8)

import Ribosome.Menu.Data.Entry (Entry (Entry))
import Ribosome.Menu.Data.Filter (Filter (Fuzzy, Prefix, Regex, Substring))
import Ribosome.Menu.Data.FilterMode (FilterMode (FilterMode))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.State (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial, Match, Refine), MenuFilter (MenuFilter))
import Ribosome.Menu.Filters (
  Matcher,
  initPar,
  matchAll,
  matchFuzzy,
  matchPrefix,
  matchRegex,
  matchSubstring,
  refinePar,
  )

execute ::
  (MenuItem i -> Maybe Text) ->
  Matcher i ->
  FilterJob i a ->
  IO a
execute extract m = \case
  Match index item ->
    pure (m extract (Entry item (fromIntegral index) False))
  Initial items ->
    initPar extract m items
  Refine entries ->
    refinePar extract m entries

-- TODO this probably needs an error type that is displayed in the status window or echoed
matcher :: MenuQuery -> Filter -> Matcher i
matcher "" =
  const matchAll
matcher (MenuQuery query) = \case
  Substring ->
    matchSubstring query
  Fuzzy ->
    matchFuzzy True (toString query)
  Prefix ->
    matchPrefix query
  Regex ->
    either (const matchAll) matchRegex (compileM (toString query) [caseless, utf8])

defaultFilter ::
  Member (Embed IO) r =>
  InterpreterFor (MenuFilter (FilterMode Filter)) r
defaultFilter =
  interpret \case
    MenuFilter (FilterMode f ex) query job ->
      embed (execute ex (matcher query f) job)
