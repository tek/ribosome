module Ribosome.Menu.Interpreter.MenuFilter where

import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (Matcher)
import Ribosome.Menu.Data.Entry (Entry (Entry))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial, Match, Refine), MenuFilter (MenuFilter))
import Ribosome.Menu.Filters (initPar, refinePar)

execute ::
  ∀ i t a .
  (MenuItem i -> Maybe t) ->
  Matcher i t ->
  FilterJob i a ->
  IO a
execute extract m = \case
  Match index item ->
    pure (m extract (Entry item (fromIntegral index) False))
  Initial items ->
    initPar extract m items
  Refine entries ->
    refinePar extract m entries

interpretFilter ::
  Member (Embed IO) r =>
  InterpreterFor MenuFilter r
interpretFilter =
  interpret \case
    MenuFilter mode query job ->
      embed (execute (MenuMode.extract mode) (MenuMode.matcher mode query) job)
