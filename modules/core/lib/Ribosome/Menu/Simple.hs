module Ribosome.Menu.Simple where

import Control.Lens (_2, over, set, (^?))
import qualified Control.Lens as Lens (element)
import Data.Composition ((.:))
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (fromList, union)
import qualified Data.Set as Set (difference, fromList, toList)
import qualified Data.Text as Text (breakOn, null)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(score, original), filter)

import Ribosome.Menu.Data.Menu (Menu(Menu), MenuFilter(MenuFilter))
import qualified Ribosome.Menu.Data.Menu as Menu (currentFilter, filtered, items, marked, selected)
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
      Fuzzy.filter text items "" "" MenuItem._text False

menuItemsNonequal :: [MenuItem i] -> [MenuItem i] -> Bool
menuItemsNonequal a b =
  (MenuItem._text <$> a) /= (MenuItem._text <$> b)

updateFilter :: MenuItemMatcher i -> Text -> Menu i -> (Bool, Bool, MenuAction m a, Menu i)
updateFilter (MenuItemMatcher matcher) text menu@(Menu items oldFiltered _ _ _ _) =
  (menuItemsNonequal filtered oldFiltered, False, MenuAction.Continue, update menu)
  where
    update =
      set Menu.filtered filtered . set Menu.currentFilter (MenuFilter text)
    filtered =
      matcher text items

reapplyFilter :: MenuItemMatcher i -> Menu i -> (Bool, Bool, MenuAction m a, Menu i)
reapplyFilter matcher menu@(Menu _ _ _ _ (MenuFilter currentFilter) _) =
  updateFilter matcher currentFilter menu

basicMenuTransform :: MenuItemMatcher i -> MenuEvent m a i -> Menu i -> (Bool, Bool, MenuAction m a, Menu i)
basicMenuTransform matcher (MenuEvent.PromptChange _ (Prompt _ _ text)) =
  set _2 True . updateFilter matcher text
basicMenuTransform _ (MenuEvent.Mapping _ _) =
  (False, False, MenuAction.Continue,)
basicMenuTransform matcher (MenuEvent.NewItems items) =
  reapplyFilter matcher . over Menu.items (++ items)
basicMenuTransform _ (MenuEvent.Init _) =
  (True, False, MenuAction.Continue,)
basicMenuTransform _ (MenuEvent.Quit reason) =
  (False, False, MenuAction.Quit reason,)

resetSelection :: Menu i -> Menu i
resetSelection (Menu i f _ _ filt mi) =
  Menu i f 0 [] filt mi

basicMenu ::
  Monad m =>
  MenuItemMatcher i ->
  (MenuUpdate m a i -> m (MenuConsumerAction m a, Menu i)) ->
  MenuUpdate m a i ->
  m (MenuAction m a, Menu i)
basicMenu matcher consumer (MenuUpdate event menu) =
  consumerAction action
  where
    (changed, action, newMenu) =
      handleReset $ basicMenuTransform matcher event menu
    handleReset (changed, True, action, newMenu) =
      (changed, action, resetSelection newMenu)
    handleReset (changed, False, action, newMenu) =
      (changed, action, newMenu)
    consumerAction (MenuAction.Quit reason) =
      return (MenuAction.Quit reason, menu)
    consumerAction _ =
      first menuAction <$> consumer (MenuUpdate event newMenu)
    menuAction MenuConsumerAction.Continue =
      if changed then MenuAction.Render True else MenuAction.Continue
    menuAction (MenuConsumerAction.Execute thunk) =
      MenuAction.Execute thunk
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
menuCycle offset m@(Menu _ filtered _ _ _ maxItems) _ =
  menuRender False (over Menu.selected add m)
  where
    count =
      maybe id min maxItems (length filtered)
    add current =
      if count == 0 then 0 else (current + offset) `mod` count

menuToggle ::
  Monad m =>
  Menu i ->
  Prompt ->
  m (MenuConsumerAction m a, Menu i)
menuToggle m@(Menu _ _ selected marked _ _) prompt =
  menuRender True . snd =<< menuCycle 1 newMenu prompt
  where
    newMenu =
      set Menu.marked newMarked m
    newMarked =
      if length removed == length marked then selected : marked else removed
    removed =
      filter (selected /=) marked

menuToggleAll ::
  Monad m =>
  Menu i ->
  Prompt ->
  m (MenuConsumerAction m a, Menu i)
menuToggleAll m@(Menu _ filtered _ marked _ _) _ =
  menuRender True newMenu
  where
    newMenu =
      set Menu.marked (Set.toList . Set.difference allIndexes $ Set.fromList marked) m
    allIndexes =
      Set.fromList [0..length filtered - 1]

defaultMappings ::
  Monad m =>
  Mappings m a i
defaultMappings =
  Map.fromList [("k", menuCycle 1), ("j", menuCycle (-1)), ("space", menuToggle), ("*", menuToggleAll)]

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

menuExecute ::
  Monad m =>
  m () ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuExecute thunk =
  return . (MenuConsumerAction.Execute thunk,)

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
selectedMenuItem (Menu _ filtered selected _ _ _) =
  filtered ^? Lens.element selected

withSelectedMenuItem ::
  Monad m =>
  (MenuItem i -> m (MenuConsumerAction m a, Menu i)) ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
withSelectedMenuItem f m =
  maybe (menuContinue m) pure =<< traverse f (selectedMenuItem m)

filterIndexes :: [Int] -> [a] -> [a]
filterIndexes indexes =
  reverse . go 0 (sort indexes) []
  where
    go current (i : is) result (a : asTail) | i == current =
      go (current + 1) is (a : result) asTail
    go current is result (_ : asTail) =
      go (current + 1) is result asTail
    go _ _ result _ =
      result

markedMenuItemsOnly :: Menu i -> Maybe [MenuItem i]
markedMenuItemsOnly (Menu _ _ _ [] _ _) =
  Nothing
markedMenuItemsOnly (Menu _ filtered _ marked _ _) =
  Just (filterIndexes marked filtered)

markedMenuItems :: Menu i -> Maybe [MenuItem i]
markedMenuItems m =
  markedMenuItemsOnly m <|> (pure <$> selectedMenuItem m)

withMarkedMenuItems ::
  Monad m =>
  ([MenuItem i] -> m (MenuConsumerAction m a, Menu i)) ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
withMarkedMenuItems f m =
  maybe (menuContinue m) pure =<< traverse f (markedMenuItems m)

traverseMarkedMenuItems ::
  Monad m =>
  (MenuItem i -> m a) ->
  Menu i ->
  m (MenuConsumerAction m [a], Menu i)
traverseMarkedMenuItems f m =
  maybe (menuContinue m) pure =<< traverse run (markedMenuItems m)
  where
    run items =
      menuQuitWith (traverse f items) m

traverseMarkedMenuItems_ ::
  Monad m =>
  (MenuItem i -> m ()) ->
  Menu i ->
  m (MenuConsumerAction m (), Menu i)
traverseMarkedMenuItems_ f m =
  first void <$> traverseMarkedMenuItems f m
