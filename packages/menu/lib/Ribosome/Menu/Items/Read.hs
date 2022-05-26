-- | Combinators for running actions on the selected or focused menu items.
-- Intended to be used from mapping handlers.
-- 'withFocus' and 'withSelection' will skip processing of the event downstream if the menu is empty.
-- Same interface as "Ribosome.Menu.Items", but using the read-only eliminator 'menuRead', allowing these actions to be
-- executed while the menu items are updated (useful for menus that get thousands of items).
module Ribosome.Menu.Items.Read where

import Control.Lens (use)

import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuState (MenuSem, menuRead, semState)
import Ribosome.Menu.ItemLens (focus, selected, selected')

-- |Run an action with the focused entry if the menu is non-empty.
withFocusItem ::
  (MenuItem i -> MenuSem i r a) ->
  MenuSem i r (Maybe a)
withFocusItem f =
  traverse f =<< semState (use focus)

-- |Run an action with the focused entry if the menu is non-empty, extracting the item payload.
withFocus' ::
  (i -> MenuSem i r a) ->
  MenuSem i r (Maybe a)
withFocus' f =
  withFocusItem (f . MenuItem._meta)

-- |Run an action with the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withFocus ::
  Members [Resource, Embed IO] r =>
  (i -> MenuSem i r (Sem r a)) ->
  MenuWidget i r a
withFocus f =
  Just . maybe MenuAction.Continue MenuAction.success <$> menuRead (withFocus' f)

withFocusM ::
  Members [Resource, Embed IO] r =>
  (i -> Sem r a) ->
  MenuWidget i r a
withFocusM f =
  withFocus (pure . f)

-- |Run an action with the selection or the focused entry if the menu is non-empty.
withSelectionItems ::
  (NonEmpty (MenuItem i) -> MenuSem i r a) ->
  MenuSem i r (Maybe a)
withSelectionItems f =
  traverse f =<< semState (use selected')

-- |Run an action with the selection or the focused entry if the menu is non-empty, extracting the item payloads.
withSelection' ::
  (NonEmpty i -> MenuSem i r a) ->
  MenuSem i r (Maybe a)
withSelection' f =
  traverse f =<< semState (use selected)

-- |Run an action with the selection or the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withSelection ::
  Members [Resource, Embed IO] r =>
  (NonEmpty i -> MenuSem i r (Sem r a)) ->
  MenuWidget i r a
withSelection f =
  Just . maybe MenuAction.Continue MenuAction.success <$> menuRead (withSelection' f)

withSelectionM ::
  Members [Resource, Embed IO] r =>
  (NonEmpty i -> Sem r a) ->
  MenuWidget i r a
withSelectionM f =
  withSelection (pure . f)

-- |Run an action with each entry in the selection or the focused entry and quit the menu with '()'.
-- If the menu was empty, do nothing (i.e. skip the event).
traverseSelection_ ::
  Members [Resource, Embed IO] r =>
  (i -> MenuSem i r ()) ->
  MenuWidget i r ()
traverseSelection_ f =
  withSelection ((unit <$) . traverse_ f)
