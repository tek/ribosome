module Ribosome.Menu.Simple where

import Control.Lens ((^?))
import qualified Control.Lens as Lens (element, over, views)
import Data.Composition ((.:))
import Data.Map ((!?))
import qualified Data.Map as Map (fromList, union)
import qualified Data.Text as Text (breakOn, length, null, take)

import Ribosome.Menu.Data.Menu (Menu(Menu), MenuFilter(MenuFilter))
import qualified Ribosome.Menu.Data.Menu as Menu (filtered, items, selected)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction (MenuAction(..))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import Ribosome.Menu.Data.MenuConsumerUpdate (MenuConsumerUpdate(MenuConsumerUpdate))
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuEvent as QuitReason (QuitReason(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import qualified Ribosome.Menu.Data.MenuUpdate as MenuUpdate (menu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

type Mappings m a = Map Text (Menu -> Prompt -> m (MenuConsumerAction m a, Menu))

textContains :: Text -> Text -> Bool
textContains needle haystack =
  Text.null needle || (not (Text.null haystack) && search needle haystack)
  where
    search =
      not . Text.null . snd .: Text.breakOn

updateFilter :: Text -> Menu -> (Bool, MenuAction m a, Menu)
updateFilter text (Menu items oldFiltered stack selected _) =
  (filtered /= oldFiltered, MenuAction.Continue, Menu items filtered stack selected (MenuFilter text))
  where
    filtered =
      filter (textContains text . MenuItem._text) items

reapplyFilter :: Menu -> (Bool, MenuAction m a, Menu)
reapplyFilter menu@(Menu _ _ _ _ (MenuFilter currentFilter)) =
  updateFilter currentFilter menu

basicMenuTransform :: MenuEvent m a -> Menu -> (Bool, MenuAction m a, Menu)
basicMenuTransform (MenuEvent.PromptChange _ (Prompt _ _ text)) =
  updateFilter text
basicMenuTransform (MenuEvent.Mapping _ _) =
  (False, MenuAction.Continue,)
basicMenuTransform (MenuEvent.NewItems item) =
  reapplyFilter . Lens.over Menu.items (item :)
basicMenuTransform (MenuEvent.Init _) =
  (True, MenuAction.Continue,)
basicMenuTransform (MenuEvent.Quit reason) =
  (False, MenuAction.Quit reason,)

basicMenu ::
  Monad m =>
  (MenuUpdate m a -> m (MenuConsumerAction m a, Menu)) ->
  MenuUpdate m a ->
  m (MenuAction m a, Menu)
basicMenu consumer (MenuUpdate event menu) =
  consumerAction action
  where
    (changed, action, newMenu) = basicMenuTransform event menu
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
  Mappings m a ->
  MenuUpdate m a ->
  m (MenuConsumerAction m a, Menu)
mappingConsumer mappings (MenuUpdate (MenuEvent.Mapping char prompt) menu) =
  handler menu prompt
  where
    handler =
      fromMaybe (const . menuContinue) (mappings !? char)
mappingConsumer _ (MenuUpdate _ menu) =
  menuContinue menu

simpleMenu ::
  Monad m =>
  Mappings m a ->
  MenuUpdate m a ->
  m (MenuAction m a, Menu)
simpleMenu =
  basicMenu . mappingConsumer

menuCycle ::
  Monad m =>
  Int ->
  Menu ->
  Prompt ->
  m (MenuConsumerAction m a, Menu)
menuCycle offset m _ =
  menuRender False (Lens.over Menu.selected add m)
  where
    count =
      Lens.views Menu.filtered length m
    add current =
      if count == 0 then 0 else (current + offset) `mod` count

defaultMappings ::
  Monad m =>
  Mappings m a
defaultMappings =
  Map.fromList [("k", menuCycle 1), ("j", menuCycle (-1))]

defaultMenu ::
  Monad m =>
  Mappings m a ->
  MenuUpdate m a ->
  m (MenuAction m a, Menu)
defaultMenu =
  simpleMenu . (`Map.union` defaultMappings)

menuContinue ::
  Monad m =>
  Menu ->
  m (MenuConsumerAction m a, Menu)
menuContinue =
  return . (MenuConsumerAction.Continue,)

menuRender ::
  Monad m =>
  Bool ->
  Menu ->
  m (MenuConsumerAction m a, Menu)
menuRender changed =
  return . (MenuConsumerAction.Render changed,)

menuQuit ::
  Monad m =>
  Menu ->
  m (MenuConsumerAction m a, Menu)
menuQuit =
  return . (MenuConsumerAction.Quit,)

menuQuitWith ::
  Monad m =>
  m a ->
  Menu ->
  m (MenuConsumerAction m a, Menu)
menuQuitWith next =
  return . (MenuConsumerAction.QuitWith next,)

menuReturn ::
  Monad m =>
  a ->
  Menu ->
  m (MenuConsumerAction m a, Menu)
menuReturn a =
  return . (MenuConsumerAction.Return a,)

selectedMenuItem :: Menu -> Maybe MenuItem
selectedMenuItem (Menu _ items _ selected _) =
  items ^? Lens.element (length items - selected - 1)
