module Ribosome.Menu.Class.MenuMode where

import Ribosome.Menu.Data.Entry (Entry)
import qualified Ribosome.Menu.Data.MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuQuery (MenuQuery)

type Extractor i a =
  MenuItem i -> Maybe a

type Matcher i a =
  Extractor i a -> Entry i -> Maybe (Int, Entry i)

class (
    Show mode,
    Ord mode
  ) => MenuMode (i :: Type) (mode :: Type) where
    type MatchOn mode :: Type
    type MatchOn _ = Text

    cycleFilter :: mode -> mode
    cycleFilter = id

    renderFilter :: mode -> Text
    renderFilter _ = "no mode"

    renderExtra :: mode -> Int -> Maybe Text
    renderExtra _ _ = Nothing

    matcher :: mode -> MenuQuery -> Matcher i (MatchOn mode)

    extract :: mode -> MenuItem i -> Maybe (MatchOn mode)
    default extract :: MatchOn mode ~ Text => mode -> MenuItem i -> Maybe (MatchOn mode)
    extract _ i = Just i.text

type ExtractFor mode i = Extractor i (MatchOn mode)

type MatcherFor mode i = Matcher i (MatchOn mode)

instance MenuMode i () where
  matcher () _ _ e = Just (0, e)
