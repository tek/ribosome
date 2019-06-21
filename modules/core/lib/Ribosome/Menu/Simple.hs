module Ribosome.Menu.Simple where

import Control.Lens ((^?))
import qualified Control.Lens as Lens (element, over, views)
import Data.Composition ((.:))
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (fromList, union)
import qualified Data.Text as Text (breakOn, null)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(score, original), filter)

import Ribosome.Menu.Data.Menu (Menu(Menu), MenuFilter(MenuFilter))
import qualified Ribosome.Menu.Data.Menu as Menu (filtered, items, selected)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction(..))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
import Ribosome.Menu.Data.MenuItemMatcher (MenuItemMatcher(MenuItemMatcher))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

type MappingHandler m a i = Menu i -> Prompt -> m (MenuConsumerAction m a, Menu i)
type Mappings m a i = Map Text (MappingHandler m a i)

textContains :: Text -> Text -> Bool
textContains needle haystack =
  Text.null needle || (not (Text.null haystack) && search needle haystack)
  where
    search =
      not . Text.null . snd .: Text.breakOn

substringMenuItemMatcher :: MenuItemMatcher a
substringMenuItemMatcher =
  MenuItemMatcher matcher
  where
    matcher text =
      filter $ textContains text . MenuItem._text

fuzzyMenuItemMatcher :: MenuItemMatcher a
fuzzyMenuItemMatcher =
  MenuItemMatcher matcher
  where
    matcher =
      fmap Fuzzy.original . sortOn Fuzzy.score .: filtered
    filtered text items =
      Fuzzy.filter text items "" "" MenuItem._text True

menuItemsNonequal :: [MenuItem i] -> [MenuItem i] -> Bool
menuItemsNonequal a b =
  (MenuItem._text <$> a) /= (MenuItem._text <$> b)

updateFilter :: MenuItemMatcher i -> Text -> Menu i -> (Bool, MenuAction m a, Menu i)
updateFilter (MenuItemMatcher matcher) text (Menu items oldFiltered stack selected _) =
  (menuItemsNonequal filtered oldFiltered, MenuAction.Continue, Menu items filtered stack selected (MenuFilter text))
  where
    filtered =
      matcher text items

reapplyFilter :: MenuItemMatcher i -> Menu i -> (Bool, MenuAction m a, Menu i)
reapplyFilter matcher menu@(Menu _ _ _ _ (MenuFilter currentFilter)) =
  updateFilter matcher currentFilter menu

basicMenuTransform :: MenuItemMatcher i -> MenuEvent m a i -> Menu i -> (Bool, MenuAction m a, Menu i)
basicMenuTransform matcher (MenuEvent.PromptChange _ (Prompt _ _ text)) =
  updateFilter matcher text
basicMenuTransform _ (MenuEvent.Mapping _ _) =
  (False, MenuAction.Continue,)
basicMenuTransform matcher (MenuEvent.NewItems items) =
  reapplyFilter matcher . Lens.over Menu.items (++ items)
basicMenuTransform _ (MenuEvent.Init _) =
  (True, MenuAction.Continue,)
basicMenuTransform _ (MenuEvent.Quit reason) =
  (False, MenuAction.Quit reason,)

basicMenu ::
  Monad m =>
  MenuItemMatcher i ->
  (MenuUpdate m a i -> m (MenuConsumerAction m a, Menu i)) ->
  MenuUpdate m a i ->
  m (MenuAction m a, Menu i)
basicMenu matcher consumer (MenuUpdate event menu) =
  consumerAction action
  where
    (changed, action, newMenu) = basicMenuTransform matcher event menu
    consumerAction (MenuAction.Quit reason) =
      return (MenuAction.Quit reason, menu)
    consumerAction _ =
      first menuAction <$> consumer (MenuUpdate event newMenu)
    menuAction MenuConsumerAction.Continue =
      if changed then MenuAction.Render True else MenuAction.Continue
    menuAction (MenuConsumerAction.Render consumerChanged) =
      MenuAction.Render (changed || consumerChanged)
    menuAction (MenuConsumerAction.QuitWith ma) =
      MenuAction.Quit (QuitReason.Execute ma)
    menuAction MenuConsumerAction.Quit =
      MenuAction.Quit QuitReason.Aborted
    menuAction (MenuConsumerAction.Return a) =
      MenuAction.Quit (QuitReason.Return a)

mappingConsumer ::
  Monad m =>
  Mappings m a i ->
  MenuUpdate m a i ->
  m (MenuConsumerAction m a, Menu i)
mappingConsumer mappings (MenuUpdate (MenuEvent.Mapping char prompt) menu) =
  handler menu prompt
  where
    handler =
      fromMaybe (const . menuContinue) (mappings !? char)
mappingConsumer _ (MenuUpdate _ menu) =
  menuContinue menu

simpleMenu ::
  Monad m =>
  Mappings m a i ->
  MenuUpdate m a i ->
  m (MenuAction m a, Menu i)
simpleMenu =
  basicMenu fuzzyMenuItemMatcher . mappingConsumer

menuCycle ::
  Monad m =>
  Int ->
  Menu i ->
  Prompt ->
  m (MenuConsumerAction m a, Menu i)
menuCycle offset m _ =
  menuRender False (Lens.over Menu.selected add m)
  where
    count =
      Lens.views Menu.filtered length m
    add current =
      if count == 0 then 0 else (current + offset) `mod` count

defaultMappings ::
  Monad m =>
  Mappings m a i
defaultMappings =
  Map.fromList [("k", menuCycle 1), ("j", menuCycle (-1))]

defaultMenu ::
  Monad m =>
  Mappings m a i ->
  MenuUpdate m a i ->
  m (MenuAction m a, Menu i)
defaultMenu =
  simpleMenu . (`Map.union` defaultMappings)

menuContinue ::
  Monad m =>
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuContinue =
  return . (MenuConsumerAction.Continue,)

menuRender ::
  Monad m =>
  Bool ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuRender changed =
  return . (MenuConsumerAction.Render changed,)

menuQuit ::
  Monad m =>
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuQuit =
  return . (MenuConsumerAction.Quit,)

menuQuitWith ::
  Monad m =>
  m a ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuQuitWith next =
  return . (MenuConsumerAction.QuitWith next,)

menuReturn ::
  Monad m =>
  a ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuReturn a =
  return . (MenuConsumerAction.Return a,)

selectedMenuItem :: Menu i -> Maybe (MenuItem i)
selectedMenuItem (Menu _ items _ selected _) =
  items ^? Lens.element (length items - selected - 1)
