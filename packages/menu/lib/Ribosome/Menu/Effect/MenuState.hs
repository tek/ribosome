module Ribosome.Menu.Effect.MenuState where

import Lens.Micro.Mtl (view)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Menu (Menu (Menu))
import Ribosome.Menu.Data.MenuItems (MenuItems)

data MenuState filter i :: Effect where
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> MenuState filter i m a
  ReadCursor :: MenuState filter i m CursorIndex
  UseItems :: (MenuItems filter i -> m (MenuItems filter i, a)) -> MenuState filter i m a
  ReadItems :: MenuState filter i m (MenuItems filter i)

makeSem ''MenuState

putCursor ::
  Member (MenuState f i) r =>
  CursorIndex ->
  Sem r ()
putCursor c =
  useCursor (const (pure (c, ())))

putItems ::
  Member (MenuState f i) r =>
  MenuItems f i ->
  Sem r ()
putItems c =
  useItems (const (pure (c, ())))

getsCursor ::
  Member (MenuState f i) r =>
  (CursorIndex -> a) ->
  Sem r a
getsCursor f =
  f <$> readCursor

getsItems ::
  Member (MenuState f i) r =>
  (MenuItems f i -> a) ->
  Sem r a
getsItems f =
  f <$> readItems

modifyCursor ::
  Member (MenuState f i) r =>
  (CursorIndex -> CursorIndex) ->
  Sem r ()
modifyCursor f =
  useCursor \ c -> pure (f c, ())

modifyItems ::
  Member (MenuState f i) r =>
  (MenuItems f i -> MenuItems f i) ->
  Sem r ()
modifyItems f =
  useItems \ i -> pure (f i, ())

viewItems ::
  Member (MenuState f i) r =>
  (SimpleGetter (MenuItems f i) a) ->
  Sem r a
viewItems g =
  view g <$> readItems

readMenu ::
  Member (MenuState f i) r =>
  Sem r (Menu f i)
readMenu =
  Menu <$> readItems <*> readCursor

viewMenu ::
  Member (MenuState f i) r =>
  (SimpleGetter (Menu f i) a) ->
  Sem r a
viewMenu g =
  view g <$> readMenu

itemsState ::
  Member (MenuState f i) r =>
  Sem (State (MenuItems f i) : r) a ->
  Sem r a
itemsState action =
  useItems \ s ->
    runState s action

type ScopedMenuState filter i =
  Scoped () (MenuState filter i)

withMenuState ::
  Member (ScopedMenuState f i) r =>
  InterpreterFor (MenuState f i) r
withMenuState =
  scoped

menuState ::
  Member (MenuState f i) r =>
  InterpreterFor (State (Menu f i)) r
menuState action =
  useCursor \ cursor ->
    useItems \ items -> do
      (Menu newItems newCursor, result) <- runState (Menu items cursor) action
      pure (newItems, (newCursor, result))

cursorState ::
  Member (MenuState f i) r =>
  Sem (State CursorIndex : Reader (MenuItems f i) : r) a ->
  Sem r a
cursorState action =
  useCursor \ cursor -> do
    items <- readItems
    runReader items (runState cursor action)
