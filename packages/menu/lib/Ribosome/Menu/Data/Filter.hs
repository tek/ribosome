module Ribosome.Menu.Data.Filter where

import Text.Regex.PCRE.Light.Char8 (caseless, compileM, utf8)

import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (Matcher, MenuMode (cycleFilter, renderExtra, renderFilter))
import Ribosome.Menu.Data.MenuQuery (MenuQuery (MenuQuery))
import Ribosome.Menu.Filters (matchAll, matchFuzzy, matchPrefix, matchRegex, matchSubstring)

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
  def = Fuzzy

-- TODO this probably needs an error type that is displayed in the status window or echoed
basicMatcher :: Filter -> MenuQuery -> Matcher i Text
basicMatcher _ "" =
  matchAll
basicMatcher f (MenuQuery query) =
  case f of
    Substring ->
      matchSubstring query
    Fuzzy ->
      matchFuzzy True (toString query)
    Prefix ->
      matchPrefix query
    Regex ->
      either (const matchAll) matchRegex (compileM (toString query) [caseless, utf8])

instance MenuMode i Filter where

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

  renderExtra _ _ =
    Nothing

  matcher = basicMatcher
