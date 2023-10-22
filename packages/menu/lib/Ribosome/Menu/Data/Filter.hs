module Ribosome.Menu.Data.Filter where

import Text.Regex.PCRE.Light.Char8 (caseless, compileM, utf8)

import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (Matcher, MenuMode (cycleFilter, renderExtra, renderFilter))
import Ribosome.Menu.Data.MenuQuery (MenuQuery (MenuQuery))
import Ribosome.Menu.Filters (matchAll, matchFuzzy, matchPrefix, matchRegex, matchSubstring)

data FilterMethod =
  Substring
  |
  Fuzzy
  |
  Prefix
  |
  Regex
  deriving stock (Eq, Show, Ord)

instance Default FilterMethod where
  def = Fuzzy

data Filter =
  Filter {
    method :: FilterMethod,
    sortLength :: Bool
  }
  deriving stock (Eq, Show, Ord)

filterWith :: FilterMethod -> Filter
filterWith method =
  Filter {method, sortLength = False}

substring :: Filter
substring = filterWith Substring

fuzzy :: Filter
fuzzy = filterWith Fuzzy

prefix :: Filter
prefix = filterWith Prefix

regex :: Filter
regex = filterWith Regex

-- TODO this probably needs an error type that is displayed in the status window or echoed
basicMatcher :: Filter -> MenuQuery -> Matcher i Text
basicMatcher _ "" =
  matchAll
basicMatcher Filter {..} (MenuQuery query) =
  case method of
    Substring ->
      matchSubstring query
    Fuzzy ->
      matchFuzzy (not sortLength) (toString query)
    Prefix ->
      matchPrefix query
    Regex ->
      either (const matchAll) matchRegex (compileM (toString query) [caseless, utf8])

instance MenuMode i Filter where

  cycleFilter Filter {..} =
    Filter {method = newMethod, ..}
    where
      newMethod = case method of
        Substring -> Fuzzy
        Fuzzy -> Prefix
        Prefix -> Regex
        Regex -> Substring

  renderFilter Filter {method} =
    case method of
      Substring -> "substring"
      Fuzzy -> "fuzzy"
      Prefix -> "prefix"
      Regex -> "regex"

  renderExtra _ _ =
    Nothing

  matcher = basicMatcher
