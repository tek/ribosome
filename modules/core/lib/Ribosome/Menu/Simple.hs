module Ribosome.Menu.Simple where

import Control.Lens (_2, element, ifolded, over, set, toListOf, view, withIndex, (^..), (^?))
import qualified Control.Lens as Lens (filtered)
import Data.Composition ((.:))
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (fromList, union)
import qualified Data.Text as Text (breakOn, null)
import qualified Text.Fuzzy as Fuzzy (Fuzzy(score, original), filter)

import Ribosome.Data.List (indexesComplement)
import Ribosome.Menu.Action (menuContinue, menuCycle, menuQuitWith, menuToggle, menuToggleAll)
import Ribosome.Menu.Data.BasicMenuAction (BasicMenuAction, BasicMenuChange)
import qualified Ribosome.Menu.Data.BasicMenuAction as BasicMenuAction (BasicMenuAction(..))
import qualified Ribosome.Menu.Data.BasicMenuAction as BasicMenuChange (BasicMenuChange(..))
import Ribosome.Menu.Data.FilteredMenuItem (FilteredMenuItem(FilteredMenuItem))
import qualified Ribosome.Menu.Data.FilteredMenuItem as FilteredMenuItem (index, item)
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
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter(MenuItemFilter))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

type MappingHandler m a i = Menu i -> Prompt -> m (MenuConsumerAction m a, Menu i)
type Mappings m a i = Map Text (MappingHandler m a i)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex =
  toListOf $ ifolded . withIndex

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
    filt text =
      uncurry FilteredMenuItem <$$> matcher text
    matcher text =
      toListOf $ ifolded . Lens.filtered (textContains text . view MenuItem.text) . withIndex

fuzzyMenuItemMatcher :: MenuItemFilter a
fuzzyMenuItemMatcher =
  MenuItemFilter matcher
  where
    matcher =
      fmap (uncurry FilteredMenuItem . Fuzzy.original) . sortOn Fuzzy.score .: filtered
    filtered text items =
      Fuzzy.filter text (items ^.. ifolded . withIndex) "" "" (view (_2 . MenuItem.text)) False

menuItemsNonequal :: [FilteredMenuItem i] -> [FilteredMenuItem i] -> Bool
menuItemsNonequal a b =
  (view FilteredMenuItem.index <$> a) /= (view FilteredMenuItem.index <$> b)

updateFilter :: MenuItemFilter i -> Text -> Menu i -> (BasicMenuChange, Menu i)
updateFilter (MenuItemFilter itemFilter) text menu@(Menu items oldFiltered _ _ _ _) =
  (change, update menu)
  where
    change =
      if menuItemsNonequal filtered oldFiltered
      then BasicMenuChange.Change
      else BasicMenuChange.NoChange
    update =
      set Menu.filtered filtered . set Menu.currentFilter (MenuFilter text)
    filtered =
      itemFilter text items

reapplyFilter :: MenuItemFilter i -> Menu i -> (BasicMenuChange, Menu i)
reapplyFilter itemFilter menu@(Menu _ _ _ _ (MenuFilter currentFilter) _) =
  updateFilter itemFilter currentFilter menu

basicMenuTransform :: MenuItemFilter i -> MenuEvent m a i -> Menu i -> BasicMenuAction m a i
basicMenuTransform matcher (MenuEvent.PromptChange _ (Prompt _ _ text)) =
  BasicMenuAction.Continue BasicMenuChange.Reset . snd . updateFilter matcher text
basicMenuTransform _ (MenuEvent.Mapping _ _) =
  BasicMenuAction.Continue BasicMenuChange.NoChange
basicMenuTransform matcher (MenuEvent.NewItems items) =
  uncurry BasicMenuAction.Continue . reapplyFilter matcher . over Menu.items (++ items)
basicMenuTransform _ (MenuEvent.Init _) =
  BasicMenuAction.Continue BasicMenuChange.Change
basicMenuTransform _ (MenuEvent.Quit reason) =
  const $ BasicMenuAction.Quit reason

resetSelection :: BasicMenuChange -> Menu i -> Menu i
resetSelection BasicMenuChange.Reset (Menu i f _ _ filt mi) =
  Menu i f 0 [] filt mi
resetSelection _ m =
  m

menuAction ::
  MenuItemFilter i ->
  Bool ->
  MenuConsumerAction m a ->
  Menu i ->
  (MenuAction m a, Menu i)
menuAction itemFilter _ MenuConsumerAction.Filter menu =
  (MenuAction.Render True, uncurry resetSelection $ reapplyFilter itemFilter menu)
menuAction _ True MenuConsumerAction.Continue menu =
  (MenuAction.Render True, menu)
menuAction _ False MenuConsumerAction.Continue menu =
  (MenuAction.Continue, menu)
menuAction _ _ (MenuConsumerAction.Execute thunk) menu =
  (MenuAction.Execute thunk, menu)
menuAction _ basicChanged (MenuConsumerAction.Render consumerChanged) menu =
  (MenuAction.Render (basicChanged || consumerChanged), menu)
menuAction _ _ (MenuConsumerAction.QuitWith ma) menu =
  (MenuAction.Quit (QuitReason.Execute ma), menu)
menuAction _ _ MenuConsumerAction.Quit menu =
  (MenuAction.Quit QuitReason.Aborted, menu)
menuAction _ _ (MenuConsumerAction.Return a) menu =
  (MenuAction.Quit (QuitReason.Return a), menu)

basicMenuAction ::
  Monad m =>
  MenuItemFilter i ->
  (MenuUpdate m a i -> m (MenuConsumerAction m a, Menu i)) ->
  MenuUpdate m a i ->
  BasicMenuAction m a i ->
  m (MenuAction m a, Menu i)
basicMenuAction itemFilter consumer (MenuUpdate event menu) =
  act
  where
    act (BasicMenuAction.Quit reason) =
      pure (MenuAction.Quit reason, menu)
    act (BasicMenuAction.Continue change m) = do
      (action, updatedMenu) <- consumerTransform (resetSelection change m)
      return $ menuAction itemFilter (change /= BasicMenuChange.NoChange) action updatedMenu
    consumerTransform newMenu =
      consumer (MenuUpdate event newMenu)

basicMenu ::
  Monad m =>
  MenuItemFilter i ->
  (MenuUpdate m a i -> m (MenuConsumerAction m a, Menu i)) ->
  MenuUpdate m a i ->
  m (MenuAction m a, Menu i)
basicMenu itemFilter consumer update@(MenuUpdate event menu) =
  basicMenuAction itemFilter consumer update basicTransform
  where
    basicTransform =
      basicMenuTransform itemFilter event menu

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

selectedMenuItem :: Menu i -> Maybe (MenuItem i)
selectedMenuItem (Menu _ filtered selected _ _ _) =
  filtered ^? element selected . FilteredMenuItem.item

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

markedIndexes :: Menu i -> [Int]
markedIndexes (Menu _ _ selected [] _ _) =
  [selected]
markedIndexes (Menu _ _ _ marked _ _) =
  marked

menuItemsByIndexes :: [Int] -> Menu i -> [MenuItem i]
menuItemsByIndexes indexes (Menu _ filtered _ _ _ _) =
  view FilteredMenuItem.item <$> filterIndexes indexes filtered

markedMenuItemsOnly :: Menu i -> Maybe (NonEmpty (MenuItem i))
markedMenuItemsOnly (Menu _ _ _ [] _ _) =
  Nothing
markedMenuItemsOnly (Menu _ filtered _ marked _ _) =
  nonEmpty $ view FilteredMenuItem.item <$> filterIndexes marked filtered

markedMenuItems :: Menu i -> Maybe (NonEmpty (MenuItem i))
markedMenuItems m =
  markedMenuItemsOnly m <|> (pure <$> selectedMenuItem m)

unmarkedMenuItems :: Menu i -> [MenuItem i]
unmarkedMenuItems menu =
  menuItemsByIndexes (indexesComplement (length (view Menu.filtered menu)) (indexes menu)) menu
  where
    indexes (Menu _ _ selected [] _ _) =
      [selected]
    indexes (Menu _ _ _ marked _ _) =
      marked

withMarkedMenuItems ::
  Monad m =>
  (NonEmpty (MenuItem i) -> m a) ->
  Menu i ->
  m (Maybe a)
withMarkedMenuItems f m =
  traverse f (markedMenuItems m)

withMarkedMenuItems_ ::
  Monad m =>
  (NonEmpty (MenuItem i) -> m ()) ->
  Menu i ->
  m ()
withMarkedMenuItems_ f m =
  traverse_ f (markedMenuItems m)

actionWithMarkedMenuItems ::
  Monad m =>
  (m (NonEmpty b) -> Menu i -> m (MenuConsumerAction m a, Menu i)) ->
  (MenuItem i -> m b) ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
actionWithMarkedMenuItems next f m =
  fromMaybe (MenuConsumerAction.Continue, m) <$> withMarkedMenuItems run m
  where
    run items =
      next (traverse f items) m

traverseMarkedMenuItems ::
  Monad m =>
  (MenuItem i -> m a) ->
  Menu i ->
  m (Maybe (NonEmpty a))
traverseMarkedMenuItems =
  withMarkedMenuItems . traverse

traverseMarkedMenuItems_ ::
  Monad m =>
  (MenuItem i -> m ()) ->
  Menu i ->
  m ()
traverseMarkedMenuItems_ =
  withMarkedMenuItems_ . traverse_

traverseMarkedMenuItemsAndQuit ::
  Monad m =>
  (MenuItem i -> m a) ->
  Menu i ->
  m (MenuConsumerAction m (NonEmpty a), Menu i)
traverseMarkedMenuItemsAndQuit =
  actionWithMarkedMenuItems menuQuitWith

traverseMarkedMenuItemsAndQuit_ ::
  Monad m =>
  (MenuItem i -> m ()) ->
  Menu i ->
  m (MenuConsumerAction m (), Menu i)
traverseMarkedMenuItemsAndQuit_ f =
  first void <$$> actionWithMarkedMenuItems menuQuitWith f

deleteByFilteredIndex :: [Int] -> Menu i -> Menu i
deleteByFilteredIndex indexes menu@(Menu items filtered _ _ _ _) =
  set Menu.items newItems . set Menu.filtered [] $ menu
  where
    newItems =
      filterIndexes (indexesComplement (length items) unfilteredIndexes) items
    unfilteredIndexes =
      view FilteredMenuItem.index <$> filterIndexes indexes filtered


deleteMarked :: Menu i -> Menu i
deleteMarked menu =
  set Menu.selected 0 . set Menu.marked [] . deleteByFilteredIndex (markedIndexes menu) $ menu
