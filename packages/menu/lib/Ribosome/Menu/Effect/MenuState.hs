module Ribosome.Menu.Effect.MenuState where

import Lens.Micro.Mtl (view)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Menu (Menu (Menu))
import Ribosome.Menu.Data.MenuData (MenuItems)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

-- TODO split in PromptControl/MenuState
data MenuState i :: Effect where
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> MenuState i m a
  ReadCursor :: MenuState i m CursorIndex
  SetPrompt :: Prompt -> MenuState i m ()
  ReadPrompt :: MenuState i m Prompt
  UseItems :: (MenuItems i -> m (MenuItems i, a)) -> MenuState i m a
  ReadItems :: MenuState i m (MenuItems i)

makeSem ''MenuState

putCursor ::
  Member (MenuState i) r =>
  CursorIndex ->
  Sem r ()
putCursor c =
  useCursor (const (pure (c, ())))

putItems ::
  Member (MenuState i) r =>
  MenuItems i ->
  Sem r ()
putItems c =
  useItems (const (pure (c, ())))

getsCursor ::
  Member (MenuState i) r =>
  (CursorIndex -> a) ->
  Sem r a
getsCursor f =
  f <$> readCursor

getsItems ::
  Member (MenuState i) r =>
  (MenuItems i -> a) ->
  Sem r a
getsItems f =
  f <$> readItems

viewCursor ::
  Member (MenuState i) r =>
  (SimpleGetter CursorIndex a) ->
  Sem r a
viewCursor g =
  view g <$> readCursor

viewItems ::
  Member (MenuState i) r =>
  (SimpleGetter (MenuItems i) a) ->
  Sem r a
viewItems g =
  view g <$> readItems

readMenu ::
  Member (MenuState i) r =>
  Sem r (Menu i)
readMenu =
  Menu <$> readItems <*> readCursor

viewMenu ::
  Member (MenuState i) r =>
  (SimpleGetter (Menu i) a) ->
  Sem r a
viewMenu g =
  view g <$> readMenu

type ScopedMenuState i =
  Scoped () (MenuState i)

withMenuState ::
  Member (ScopedMenuState i) r =>
  InterpreterFor (MenuState i) r
withMenuState =
  scoped
