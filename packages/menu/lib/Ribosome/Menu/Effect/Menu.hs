module Ribosome.Menu.Effect.Menu where

import Lens.Micro.Mtl (view)
import Polysemy.Bundle (Bundle, sendBundle)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Class.MenuState (Item, MenuState (core))
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.State (Core, ModalState)
import Ribosome.Menu.Data.WindowConfig (WindowConfig)
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data Menu s :: Effect where
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> Menu s m a
  ReadCursor :: Menu s m CursorIndex
  UseState :: (s -> m (s, a)) -> Menu s m a
  ReadState :: Menu s m s

makeSem ''Menu

data MenuCore :: Effect where
  StartPrompt :: MenuCore m ()
  WaitPrompt :: MenuCore m ()
  PromptQuit :: MenuCore m ()
  PromptUpdated :: Prompt -> MenuCore m ()
  PromptLooped :: MenuCore m ()
  Render :: MenuCore m ()

makeSem ''MenuCore

type MenuEngineStack s =
  [Menu s, MenuUi, MenuCore]

newtype MenuEngine s m a =
  MenuEngine { unMenuEngine :: Bundle (MenuEngineStack s) m a }

menuEngine ::
  ∀ e s r .
  Member (MenuEngine s) r =>
  Member e (MenuEngineStack s) =>
  InterpreterFor e r
menuEngine =
  transform MenuEngine .
  sendBundle .
  raiseUnder @(Bundle (MenuEngineStack s))

bundleMenuEngine ::
  ∀ s r .
  Member (MenuEngine s) r =>
  InterpretersFor (MenuEngineStack s) r
bundleMenuEngine =
  menuEngine @MenuCore .
  menuEngine @MenuUi .
  menuEngine @(Menu s)

type ModalMenu i =
  Menu (ModalState i)

type ItemStream s =
  SerialT IO (MenuItem (Item s))

data MenuParams s =
  MenuParams {
    items :: ItemStream s,
    initialState :: s
  }

type Menus s =
  Scoped (MenuParams s) (MenuEngine s)

data UiMenuParams ui s =
  UiMenuParams {
    basic :: MenuParams s,
    ui :: ui
  }

type UiMenus ui s =
  Scoped (UiMenuParams ui s) (MenuEngine s)

type WindowMenus s =
  UiMenus WindowConfig s

type ModalMenus i =
  Menus (ModalState i)

type ModalUiMenus ui i =
  UiMenus ui (ModalState i)

type ModalWindowMenus i =
  WindowMenus (ModalState i)

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
  InterpreterFor (State (Core (Item s))) r
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
      (WithCursor newItems newCursor,ult) <- runState (WithCursor s cursor) action
      pure (newItems, (newCursor,ult))
