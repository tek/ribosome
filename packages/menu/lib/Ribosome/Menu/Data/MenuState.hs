module Ribosome.Menu.Data.MenuState where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVarIO, readTVar, writeTVar)
import Control.Lens ((.~), (^.))
import qualified Control.Monad.State as State
import Control.Monad.State (MonadState)
import qualified Polysemy.Conc as Sync

import Ribosome.Menu.Data.Menu (Menu (Menu), cursor, prompt)
import Ribosome.Menu.Data.MenuData (MenuCursor, MenuItems, dirty)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data ItemsLock =
  ItemsLock
  deriving stock (Eq, Show)

itemsLock ::
  Members [Sync ItemsLock, Resource] r =>
  Sem r a ->
  Sem r a
itemsLock =
  Sync.lock ItemsLock

cursorLock ::
  Members [Sync CursorLock, Resource] r =>
  Sem r a ->
  Sem r a
cursorLock =
  Sync.lock CursorLock

data CursorLock =
  CursorLock
  deriving stock (Eq, Show)

data MenuState i =
  MenuState {
    menuItems :: TVar (MenuItems i),
    menuCursor :: TVar MenuCursor,
    menuPrompt :: TVar Prompt
  }

type MenuStateSem r i a =
  Sem (Reader (MenuState i) : Sync ItemsLock : Sync CursorLock : r) a

type MenuSem r i a =
  Sem (State (Menu i) : r) a

type MenuItemsSem r i a =
  Sem (State (MenuItems i) : r) a

newtype SemS s r a =
  SemS { unSemS :: Sem (State s : r) a }
  deriving newtype (Functor, Applicative, Monad)

type MenuItemsSemS r i a =
  SemS (MenuItems i) r a

semState :: SemS s r a -> Sem (State s : r) a
semState =
  unSemS

instance MonadState s (SemS s r) where
  get = SemS get
  put = SemS . put

newMenuState ::
  Member (Embed IO) r =>
  Sem r (MenuState i)
newMenuState =
  embed do
    menuItems <- newTVarIO def
    menuCursor <- newTVarIO def
    menuPrompt <- newTVarIO def
    pure MenuState {..}

readMenuSTM ::
  MenuState i ->
  STM (Menu i)
readMenuSTM MenuState {..} =
  Menu <$> readTVar menuItems <*> readTVar menuCursor <*> readTVar menuPrompt

readMenu ::
  MonadIO m =>
  MenuState i ->
  m (Menu i)
readMenu =
  liftIO . atomically . readMenuSTM

readMenuForRender ::
  MonadIO m =>
  MenuState i ->
  m (Menu i)
readMenuForRender menuState@MenuState {..} =
  liftIO $ atomically do
    m <- readMenuSTM menuState
    modifyTVar menuItems (dirty .~ False)
    pure m

modifyMenuCursor ::
  Members [Sync ItemsLock, Sync CursorLock, Resource, Embed IO] r =>
  MenuState i ->
  Sem (Reader (Menu i) : r) (a, MenuCursor) ->
  Sem r a
modifyMenuCursor menuState@MenuState {menuCursor} f =
  cursorLock do
    menu <- readMenu menuState
    (result, newCursor) <- runReader menu f
    embed $ atomically do
      writeTVar menuCursor newCursor
    pure result

modifyMenuItems ::
  Members [Sync ItemsLock, Sync CursorLock, Resource, Embed IO] r =>
  MenuState i ->
  Sem (Reader (Menu i) : r) (MenuItems i, a) ->
  Sem r a
modifyMenuItems menuState@MenuState {menuItems} f =
  itemsLock do
    menu <- readMenu menuState
    (newItems, result) <- runReader menu f
    embed $ atomically do
      writeTVar menuItems newItems
      modifyTVar menuItems (dirty .~ True)
    pure result

menuItemsStateSem ::
  Members [Sync ItemsLock, Sync CursorLock, Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> MenuCursor -> MenuItemsSem r i a) ->
  Sem r a
menuItemsStateSem menuState f =
  modifyMenuItems menuState do
    Menu i s p <- ask
    raise (runState i (f p s))

runMenuM ::
  Members [Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> Sem (State (Menu i) : r) a) ->
  Sem (Sync ItemsLock : Sync CursorLock : r) a
runMenuM menuState@MenuState {..} f =
  itemsLock do
    cursorLock do
      menu <- readMenu menuState
      (Menu newItems newState _, result) <- insertAt @0 (runState menu (f (menu ^. prompt)))
      embed $ atomically do
        writeTVar menuItems newItems
        writeTVar menuCursor newState
        modifyTVar menuItems (dirty .~ True)
      pure result

runMenuMRead ::
  Members [Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> MenuSem r i a) ->
  Sem (Sync ItemsLock : Sync CursorLock : r) a
runMenuMRead menuState@MenuState {menuCursor} action =
  cursorLock do
    menu <- readMenu menuState
    (newMenu, a) <- insertAt @0 (runState menu (action (menu ^. prompt)))
    a <$ embed (atomically (writeTVar menuCursor (newMenu ^. cursor)))

setPrompt ::
  Member (Embed IO) r =>
  MenuState i ->
  Prompt ->
  Sem r ()
setPrompt MenuState {menuPrompt} p =
  embed (atomically (writeTVar menuPrompt p))

menuWrite ::
  Members [Resource, Embed IO] r =>
  MenuSem r i a ->
  MenuStateSem r i a
menuWrite action = do
  menuState <- ask
  insertAt @0 (runMenuM menuState (const action))

menuRead ::
  Members [Resource, Embed IO] r =>
  MenuSem r i a ->
  MenuStateSem r i a
menuRead action = do
  menuState <- ask
  insertAt @0 (runMenuMRead menuState (const action))
