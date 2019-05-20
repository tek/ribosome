module Ribosome.Menu.Simple where

import qualified Control.Lens as Lens (over, views)
import Data.Composition ((.:))
import Data.Map ((!?))
import qualified Data.Map as Map (fromList, union)
import qualified Data.Text as Text (breakOn, length, null, take)

import Ribosome.Menu.Data.Menu (Menu(Menu), MenuFilter(MenuFilter))
import qualified Ribosome.Menu.Data.Menu as Menu (filtered, items, selected)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

type Mappings m a = Map Text (Menu -> Prompt -> m (MenuConsumerAction m a, Menu))

textContains :: Text -> Text -> Bool
textContains needle haystack =
  (Text.null needle) || (not (Text.null haystack) && search needle haystack)
  where
    search =
      not . Text.null . snd .: Text.breakOn

updateFilter :: Text -> Menu -> Menu
updateFilter text (Menu items _ stack selected _) =
  Menu items filtered stack selected (MenuFilter text)
  where
    filtered =
      filter (textContains text . MenuItem._text) items

reapplyFilter :: Menu -> Menu
reapplyFilter menu@(Menu _ _ _ _ (MenuFilter currentFilter)) =
  updateFilter currentFilter menu

basicMenu ::
  Monad m =>
  (MenuUpdate m a -> m (MenuConsumerAction m a, Menu)) ->
  MenuUpdate m a ->
  m (MenuConsumerAction m a, Menu)
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
  m (MenuConsumerAction m a, Menu)
simpleMenu =
  basicMenu . mappingConsumer

menuCycle ::
  Monad m =>
  Int ->
  Menu ->
  Prompt ->
  m (MenuConsumerAction m a, Menu)
menuCycle offset m _ =
  menuContinue (Lens.over Menu.selected add m)
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
  m (MenuConsumerAction m a, Menu)
defaultMenu =
  simpleMenu . (`Map.union` defaultMappings)

menuContinue ::
  Monad m =>
  Menu ->
  m (MenuConsumerAction m a, Menu)
menuContinue =
  return . (MenuConsumerAction.Continue,)

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
