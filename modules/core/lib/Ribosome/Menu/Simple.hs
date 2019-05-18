module Ribosome.Menu.Simple where

import qualified Control.Lens as Lens (over, views)
import Data.Map ((!?))
import qualified Data.Map as Map (fromList, union)
import qualified Data.Text as Text (length, take)

import Ribosome.Menu.Data.Menu (Menu(Menu), MenuFilter(MenuFilter))
import qualified Ribosome.Menu.Data.Menu as Menu (filtered, items, selected)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

type Mappings m = Map Text (Menu -> Prompt -> m (MenuConsumerAction m, Menu))

updateFilter :: Text -> Menu -> Menu
updateFilter text (Menu items _ stack selected _) =
  Menu items filtered stack selected (MenuFilter text)
  where
    filtered =
      filter ((text ==) . prefix . MenuItem.text) items
    prefix =
      Text.take (Text.length text)

reapplyFilter :: Menu -> Menu
reapplyFilter menu@(Menu _ _ _ _ (MenuFilter currentFilter)) =
  updateFilter currentFilter menu

basicMenu ::
  Monad m =>
  (MenuUpdate -> m (MenuConsumerAction m, Menu)) ->
  MenuUpdate ->
  m (MenuConsumerAction m, Menu)
basicMenu consumer (MenuUpdate event menu) =
  consumer . MenuUpdate event . transform event $ menu
  where
    transform (MenuEvent.PromptChange _ (Prompt _ _ text)) =
      updateFilter text
    transform (MenuEvent.Mapping _ _) =
      id
    transform (MenuEvent.NewItems item) =
      reapplyFilter . Lens.over Menu.items (item :)
    transform (MenuEvent.Init _) =
      id
    transform (MenuEvent.Quit _) =
      id

mappingConsumer ::
  Monad m =>
  Mappings m ->
  MenuUpdate ->
  m (MenuConsumerAction m, Menu)
mappingConsumer mappings (MenuUpdate (MenuEvent.Mapping char prompt) menu) =
  handler menu prompt
  where
    handler =
      fromMaybe (const . menuContinue) (mappings !? char)
mappingConsumer _ (MenuUpdate _ menu) =
  menuContinue menu

simpleMenu ::
  Monad m =>
  Mappings m ->
  MenuUpdate ->
  m (MenuConsumerAction m, Menu)
simpleMenu =
  basicMenu . mappingConsumer

menuCycle ::
  Monad m =>
  Int ->
  Menu ->
  Prompt ->
  m (MenuConsumerAction m, Menu)
menuCycle offset m _ =
  menuContinue (Lens.over Menu.selected add m)
  where
    add current =
      (current + offset) `mod` Lens.views Menu.filtered length m

defaultMappings ::
  Monad m =>
  Mappings m
defaultMappings =
  Map.fromList [("k", menuCycle 1), ("j", menuCycle (-1))]

defaultMenu ::
  Monad m =>
  Mappings m ->
  MenuUpdate ->
  m (MenuConsumerAction m, Menu)
defaultMenu =
  simpleMenu . (`Map.union` defaultMappings)

menuContinue ::
  Monad m =>
  Menu ->
  m (MenuConsumerAction m, Menu)
menuContinue =
  return . (MenuConsumerAction.Continue,)

menuQuit ::
  Monad m =>
  Menu ->
  m (MenuConsumerAction m, Menu)
menuQuit =
  return . (MenuConsumerAction.Quit,)

menuQuitWith ::
  Monad m =>
  m () ->
  Menu ->
  m (MenuConsumerAction m, Menu)
menuQuitWith next =
  return . (MenuConsumerAction.QuitWith next,)
