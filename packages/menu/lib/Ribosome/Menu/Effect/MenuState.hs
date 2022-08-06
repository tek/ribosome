module Ribosome.Menu.Effect.MenuState where

import Lens.Micro.Mtl (view)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Menu (Menu (Menu))
import Ribosome.Menu.Data.MenuItems (MenuItems)

data MenuState i :: Effect where
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> MenuState i m a
  ReadCursor :: MenuState i m CursorIndex
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

modifyCursor ::
  Member (MenuState i) r =>
  (CursorIndex -> CursorIndex) ->
  Sem r ()
modifyCursor f =
  useCursor \ c -> pure (f c, ())

modifyItems ::
  Member (MenuState i) r =>
  (MenuItems i -> MenuItems i) ->
  Sem r ()
modifyItems f =
  useItems \ i -> pure (f i, ())

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

itemsState ::
  Member (MenuState i) r =>
  Sem (State (MenuItems i) : r) a ->
  Sem r a
itemsState action =
  useItems \ s ->
    runState s action

type ScopedMenuState i =
  Scoped () (MenuState i)

withMenuState ::
  Member (ScopedMenuState i) r =>
  InterpreterFor (MenuState i) r
withMenuState =
  scoped

menuState ::
  Member (MenuState i) r =>
  InterpreterFor (State (Menu i)) r
menuState action =
  useCursor \ cursor ->
    useItems \ items -> do
      (Menu newItems newCursor, result) <- runState (Menu items cursor) action
      pure (newItems, (newCursor, result))

cursorState ::
  Member (MenuState i) r =>
  Sem (State CursorIndex : Reader (MenuItems i) : r) a ->
  Sem r a
cursorState action =
  useCursor \ cursor -> do
    items <- readItems
    runReader items (runState cursor action)
