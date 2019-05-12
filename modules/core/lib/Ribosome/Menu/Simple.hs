module Ribosome.Menu.Simple where

import qualified Control.Lens as Lens (over)
import Data.Map ((!?))
import qualified Data.Text as Text (length, take)

import Ribosome.Menu.Data.Menu (Menu(Menu))
import qualified Ribosome.Menu.Data.Menu as Menu (items)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

type Mappings m = Map Text (Menu -> Prompt -> m Menu)

updateFilter :: Text -> Prompt -> Menu -> Menu
updateFilter char (Prompt cursor _ text) (Menu items _ stack) =
  Menu items (filter ((text ==) . prefix . MenuItem.text) items) stack
  where
    prefix =
      Text.take (Text.length text)

basicMenu ::
  Monad m =>
  (MenuUpdate -> m Menu) ->
  MenuUpdate ->
  m Menu
basicMenu consumer (MenuUpdate event menu) =
  consumer . MenuUpdate event . transform event $ menu
  where
    transform (MenuEvent.PromptChange char prompt) =
      updateFilter char prompt
    transform (MenuEvent.Mapping char prompt) =
      id
    transform (MenuEvent.NewItems item) =
      Lens.over Menu.items (item :)
    transform MenuEvent.Quit =
      id

mappingConsumer ::
  Monad m =>
  Mappings m ->
  MenuUpdate ->
  m Menu
mappingConsumer mappings (MenuUpdate (MenuEvent.Mapping char prompt) menu) =
  handler menu prompt
  where
    handler =
      fromMaybe (const . return) (mappings !? char)
mappingConsumer _ (MenuUpdate _ menu) =
  return menu

simpleMenu ::
  Monad m =>
  Mappings m ->
  MenuUpdate ->
  m Menu
simpleMenu =
  basicMenu . mappingConsumer

renderNvimMenu :: MenuUpdate -> m ()
renderNvimMenu =
  undefined
