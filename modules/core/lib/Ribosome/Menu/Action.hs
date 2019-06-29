module Ribosome.Menu.Action where

import Control.Lens (over, set)

import Ribosome.Data.List (indexesComplement)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import qualified Ribosome.Menu.Data.Menu as Menu (marked, selected)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuConsumerAction as MenuConsumerAction (MenuConsumerAction(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))

menuContinue ::
  Applicative m =>
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuContinue =
  return . (MenuConsumerAction.Continue,)

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
  a ->
  Menu i ->
  m (MenuConsumerAction m a, Menu i)
menuFilter a =
  pure . (MenuConsumerAction.Filter,)

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
      set Menu.marked (indexesComplement (length filtered) marked) m
