module Ribosome.Menu.Effect.Menu where

import Conc (PScoped)
import Lens.Micro.Mtl (view)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Class.MenuState (Item, MenuState (core))
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.State (Core, ModalState)
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data Menu s :: Effect where
  WithRender :: (RenderMenu (Item s) -> m ()) -> m a -> Menu s m a
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> Menu s m a
  ReadCursor :: Menu s m CursorIndex
  UseState :: (s -> m (s, a)) -> Menu s m a
  ReadState :: Menu s m s
  StartPrompt :: Menu s m ()
  WaitPrompt :: Menu s m ()
  PromptQuit :: Menu s m ()
  PromptUpdated :: Prompt -> Menu s m ()
  PromptLooped :: Menu s m ()
  Render :: Menu s m ()

makeSem ''Menu

type ModalMenu i =
  Menu (ModalState i)

type Menus s =
  PScoped (SerialT IO (MenuItem (Item s)), s) () (Menu s)

type ModalMenus i =
  Menus (ModalState i)

putCursor ::
  Member (Menu s) r =>
  CursorIndex ->
  Sem r ()
putCursor c =
  useCursor (const (pure (c, ())))

putState ::
  Member (Menu s) r =>
  s ->
  Sem r ()
putState c =
  useState (const (pure (c, ())))

getsCursor ::
  Member (Menu s) r =>
  (CursorIndex -> a) ->
  Sem r a
getsCursor f =
  f <$> readCursor

getsState ::
  Member (Menu s) r =>
  (s -> a) ->
  Sem r a
getsState f =
  f <$> readState

modifyCursor ::
  Member (Menu s) r =>
  (CursorIndex -> CursorIndex) ->
  Sem r ()
modifyCursor f =
  useCursor \ c -> pure (f c, ())

modifyState ::
  Member (Menu s) r =>
  (s -> s) ->
  Sem r ()
modifyState f =
  useState \ i -> pure (f i, ())

readCore ::
  MenuState s =>
  Member (Menu s) r =>
  Sem r (Core (Item s))
readCore =
  view core <$> readState

viewState ::
  Member (Menu s) r =>
  (SimpleGetter s a) ->
  Sem r a
viewState g =
  view g <$> readState

viewCore ::
  MenuState s =>
  Member (Menu s) r =>
  (SimpleGetter (Core (Item s)) a) ->
  Sem r a
viewCore g =
  view g <$> readCore

readMenu ::
  Member (Menu s) r =>
  Sem r (WithCursor s)
readMenu =
  WithCursor <$> readState <*> readCursor

viewMenu ::
  Member (Menu s) r =>
  (SimpleGetter (WithCursor s) a) ->
  Sem r a
viewMenu g =
  view g <$> readMenu

basicState ::
  MenuState s =>
  Member (Menu s) r =>
  Sem (State (Core (Item s)) : r) a ->
  Sem r a
basicState action =
  useState \ s -> do
    (new, a) <- runState (s ^. core) action
    pure (s & core .~ new, a)

menuState ::
  Member (Menu s) r =>
  InterpreterFor (State (WithCursor s)) r
menuState action =
  useCursor \ cursor ->
    useState \ s -> do
      (WithCursor newItems newCursor, result) <- runState (WithCursor s cursor) action
      pure (newItems, (newCursor, result))
