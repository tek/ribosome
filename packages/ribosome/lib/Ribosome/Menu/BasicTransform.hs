module Ribosome.Menu.BasicTransform where

import Control.Lens (ifolded, set, toListOf, view, withIndex)
import qualified Control.Lens as Lens (filtered)
import Data.Composition ((.:))
import qualified Data.Text as Text
import Text.FuzzyFind (Alignment (Alignment), Score, bestMatch)

import Ribosome.Menu.Data.BasicMenuAction (BasicMenuAction, BasicMenuChange)
import qualified Ribosome.Menu.Data.BasicMenuAction as BasicMenuAction (BasicMenuAction (..))
import qualified Ribosome.Menu.Data.BasicMenuAction as BasicMenuChange (BasicMenuChange (..))
import qualified Ribosome.Menu.Data.FilteredMenuItem as FilteredMenuItem
import Ribosome.Menu.Data.FilteredMenuItem (FilteredMenuItem (FilteredMenuItem))
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.Menu (Menu (Menu), MenuFilter (MenuFilter), _currentQuery)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter (MenuItemFilter))
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  lastChange,
  )

textContains :: Text -> Text -> Bool
textContains needle haystack =
  Text.null needle || (not (Text.null haystack) && search needle haystack)
  where
    search =
      not . Text.null . snd .: Text.breakOn

substringMenuItemMatcher :: MenuItemFilter a
substringMenuItemMatcher =
  MenuItemFilter filt
  where
    filt query =
      fmap (uncurry FilteredMenuItem) <$> matcher query
    matcher query =
      toListOf $ ifolded . Lens.filtered (textContains query . view MenuItem.text) . withIndex

filterFuzzy ::
  String ->
  [MenuItem a] ->
  [(Down Score, FilteredMenuItem a)]
filterFuzzy query items =
  mapMaybe (uncurry match) (zip [0..] items)
  where
    match index item@(MenuItem _ (toString -> text) _) = do
      Alignment score _ <- bestMatch query text
      pure (Down score, (FilteredMenuItem index item))

fuzzyMenuItemMatcher :: MenuItemFilter a
fuzzyMenuItemMatcher =
  MenuItemFilter \ (toString -> query) items ->
    snd <$> sortOn fst (filterFuzzy query items)

menuItemsNonequal :: [FilteredMenuItem i] -> [FilteredMenuItem i] -> Bool
menuItemsNonequal a b =
  (view FilteredMenuItem.index <$> a) /= (view FilteredMenuItem.index <$> b)

updateFiltered ::
  MenuItemFilter i ->
  Text ->
  Menu i ->
  [MenuItem i] ->
  (BasicMenuChange, Menu i)
updateFiltered (MenuItemFilter itemFilter) query menu items =
  (change, update menu)
  where
    change =
      if menuItemsNonequal newFiltered lastFiltered
      then BasicMenuChange.Reset
      else BasicMenuChange.NoChange
    update =
      Menu.push newFiltered . set Menu.currentQuery (MenuFilter query)
    newFiltered =
      itemFilter query items
    lastFiltered =
      fold (menu ^. Menu.filtered)

resetFiltered :: MenuItemFilter i -> Text -> Menu i -> (BasicMenuChange, Menu i)
resetFiltered itemFilter query menu =
  updateFiltered itemFilter query (menu & Menu.history .~ []) (menu ^. Menu.items)

popFiltered :: MenuItemFilter i -> Text -> Menu i -> (BasicMenuChange, Menu i)
popFiltered itemFilter query menu =
  case menu ^. Menu.history of
    (f : fs) -> (BasicMenuChange.Reset, menu & Menu.history .~ fs & Menu.filtered ?~ f)
    [] -> resetFiltered itemFilter query menu

reapplyFilter :: MenuItemFilter i -> Menu i -> (BasicMenuChange, Menu i)
reapplyFilter itemFilter menu@(Menu { _currentQuery = MenuFilter currentQuery}) =
  resetFiltered itemFilter currentQuery menu

promptChange :: MenuItemFilter i -> Prompt -> Menu i -> PromptChange -> (BasicMenuChange, Menu i)
promptChange itemFilter (Prompt _ _ query _) menu = \case
  PromptAppend ->
    case menu ^. Menu.filtered of
      Just cur ->
        updateFiltered itemFilter query menu (FilteredMenuItem._item <$> cur)
      Nothing ->
        resetFiltered itemFilter query menu
  PromptUnappend ->
    popFiltered itemFilter query menu
  PromptRandom ->
    resetFiltered itemFilter query menu

-- TODO optimize
newItems :: MenuItemFilter i -> [MenuItem i] -> Menu i -> (BasicMenuChange, Menu i)
newItems itemFilter new menu =
  reapplyFilter itemFilter (menu & Menu.items %~ (new ++))

basicMenuTransform ::
  MenuItemFilter i ->
  Menu i ->
  MenuEvent m a i ->
  BasicMenuAction m a i
basicMenuTransform itemFilter menu = \case
  MenuEvent.PromptChange prompt ->
    uncurry BasicMenuAction.Continue (promptChange itemFilter prompt menu (prompt ^. lastChange))
  MenuEvent.Mapping _ _ ->
    BasicMenuAction.Continue BasicMenuChange.NoChange menu
  MenuEvent.NewItems items ->
    uncurry BasicMenuAction.Continue (newItems itemFilter items menu)
  MenuEvent.Init _ ->
    BasicMenuAction.Continue BasicMenuChange.Change menu
  MenuEvent.Quit reason ->
    BasicMenuAction.Quit reason
