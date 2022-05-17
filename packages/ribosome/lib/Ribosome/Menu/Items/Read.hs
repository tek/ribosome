-- | Combinators for running actions on the selected or focused menu items.
-- Intended to be used from mapping handlers.
-- 'withFocus' and 'withSelection' will skip processing of the event downstream if the menu is empty.
-- Same interface as "Ribosome.Menu.Items", but using the read-only eliminator 'menuRead', allowing these actions to be
-- executed while the menu items are updated (useful for menus that get thousands of items).
module Ribosome.Menu.Items.Read where

import Control.Lens (use)
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuState (MenuM, menuRead)
import Ribosome.Menu.ItemLens (focus, selected, selected')

-- |Run an action with the focused entry if the menu is non-empty.
withFocusItem ::
  Monad m =>
  (MenuItem i -> MenuM m i a) ->
  MenuM m i (Maybe a)
withFocusItem f =
  traverse f =<< use focus

-- |Run an action with the focused entry if the menu is non-empty, extracting the item payload.
withFocus' ::
  Monad m =>
  (i -> MenuM m i a) ->
  MenuM m i (Maybe a)
withFocus' f =
  withFocusItem (f . MenuItem._meta)

-- |Run an action with the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withFocus ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (i -> MenuM m i (m a)) ->
  MenuWidget m i a
withFocus f =
  Just . maybe MenuAction.Continue MenuAction.success <$> menuRead (withFocus' f)

withFocusM ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (i -> m a) ->
  MenuWidget m i a
withFocusM f =
  withFocus (pure . f)

-- |Run an action with the selection or the focused entry if the menu is non-empty.
withSelectionItems ::
  Monad m =>
  (NonEmpty (MenuItem i) -> MenuM m i a) ->
  MenuM m i (Maybe a)
withSelectionItems f =
  traverse f =<< use selected'

-- |Run an action with the selection or the focused entry if the menu is non-empty, extracting the item payloads.
withSelection' ::
  Monad m =>
  (NonEmpty i -> MenuM m i a) ->
  MenuM m i (Maybe a)
withSelection' f =
  traverse f =<< use selected

-- |Run an action with the selection or the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withSelection ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (NonEmpty i -> MenuM m i (m a)) ->
  MenuWidget m i a
withSelection f =
  Just . maybe MenuAction.Continue MenuAction.success <$> menuRead (withSelection' f)

withSelectionM ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (NonEmpty i -> m a) ->
  MenuWidget m i a
withSelectionM f =
  withSelection (pure . f)

-- |Run an action with each entry in the selection or the focused entry and quit the menu with '()'.
-- If the menu was empty, do nothing (i.e. skip the event).
traverseSelection_ ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (i -> MenuM m i ()) ->
  MenuWidget m i ()
traverseSelection_ f =
  withSelection ((unit <$) . traverse_ f)
