module Ribosome.Menu.Action where

import Ribosome.Data.List (indexesComplement)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.Menu (Menu (Menu), current)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

menuContinue ::
  Applicative m =>
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuContinue =
  pure . (MenuConsumerAction.Continue,)

menuExecute ::
  Applicative m =>
  m () ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuExecute thunk =
  pure . (MenuConsumerAction.Execute thunk,)

menuRender ::
  Applicative m =>
  Bool ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuRender changed =
  pure . (MenuConsumerAction.Render changed,)

menuQuit ::
  Applicative m =>
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuQuit =
  pure . (MenuConsumerAction.Quit,)

menuQuitWith ::
  Applicative m =>
  m a ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuQuitWith next =
  pure . (MenuConsumerAction.QuitWith next,)

menuReturn ::
  Applicative m =>
  a ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuReturn a =
  pure . (MenuConsumerAction.Return a,)

menuFilter ::
  Applicative m =>
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuFilter =
  pure . (MenuConsumerAction.Filter,)

menuCycle ::
  Monad m =>
  Int ->
  Menu i ->
  Prompt ->
  m (MenuConsumerAction m a, Menu i)
menuCycle offset m _ =
  menuRender False (m & Menu.selected %~ add)
  where
    add currentCount =
      if count == 0 then 0 else (currentCount + offset) `mod` count
    count =
      maybe id min (m ^. Menu.maxItems) (length (m ^. current))

menuToggle ::
  Monad m =>
  Menu i ->
  Prompt ->
  m (MenuConsumerAction m a, Menu i)
menuToggle m@(Menu _ _ _ selected marked _ _) prompt =
  menuRender True . snd =<< menuCycle 1 newMenu prompt
  where
    newMenu =
      m & Menu.marked .~ newMarked
    newMarked =
      if length removed == length marked then selected : marked else removed
    removed =
      filter (selected /=) marked

menuToggleAll ::
  Monad m =>
  Menu i ->
  Prompt ->
  m (MenuConsumerAction m a, Menu i)
menuToggleAll m@(Menu _ (Just filtered) _ _ marked _ _) _ =
  menuRender True newMenu
  where
    newMenu =
      m & Menu.marked .~ indexesComplement (length filtered) marked
menuToggleAll m _ =
  menuContinue m

menuUpdatePrompt ::
  Applicative m =>
  Prompt ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuUpdatePrompt prompt =
  pure . (MenuConsumerAction.UpdatePrompt prompt,)
