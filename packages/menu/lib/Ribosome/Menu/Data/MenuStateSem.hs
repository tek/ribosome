module Ribosome.Menu.Data.MenuStateSem where

import Control.Concurrent.STM (
  STM,
  TVar,
  atomically,
  modifyTVar,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
  )
import Control.Lens ((.~), (^.))
import qualified Control.Monad.State as State
import Control.Monad.State (MonadState)
import qualified Polysemy.Conc as Sync
import Prelude hiding (newMVar, withMVar)

import Ribosome.Menu.Data.Menu (Menu (Menu), cursor)
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
  Members [Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> MenuCursor -> MenuItems i -> Sem r (a, MenuCursor)) ->
  Sem (Sync ItemsLock : Sync CursorLock : r) a
modifyMenuCursor menuState@MenuState {menuCursor} f =
  Sync.lock CursorLock do
    Menu items cur prompt <- readMenu menuState
    (result, newCursor) <- insertAt @0 (f prompt cur items)
    embed $ atomically do
      writeTVar menuCursor newCursor
    pure result

modifyMenuItems ::
  Members [Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> MenuCursor -> MenuItems i -> Sem r (MenuItems i, a)) ->
  Sem (Sync ItemsLock : Sync CursorLock : r) a
modifyMenuItems menuState@MenuState {menuItems} f =
  itemsLock do
    Menu items st prompt <- readMenu menuState
    (newItems, result) <- insertAt @0 (f prompt st items)
    embed $ atomically do
      writeTVar menuItems newItems
      modifyTVar menuItems (dirty .~ True)
    pure result

menuItemsStateSem ::
  Members [Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> MenuCursor -> MenuItemsSem r i a) ->
  Sem (Sync ItemsLock : Sync CursorLock : r) a
menuItemsStateSem menuState f =
  modifyMenuItems menuState \ p s i -> runState i (f p s)

runMenuM ::
  Members [Resource, Embed IO] r =>
  MenuState i ->
  (Prompt -> Sem (State (Menu i) : r) a) ->
  Sem (Sync ItemsLock : Sync CursorLock : r) a
runMenuM menuState@MenuState {..} f =
  Sync.lock ItemsLock do
    Sync.lock CursorLock do
      menu <- readMenu menuState
      prompt <- embed (readTVarIO menuPrompt)
      (Menu newItems newState _, result) <- insertAt @0 (runState menu (f prompt))
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
runMenuMRead menuState@MenuState {..} action =
  Sync.lock CursorLock do
    menu <- readMenu menuState
    prompt <- embed (readTVarIO menuPrompt)
    (newMenu, a) <- insertAt @0 (runState menu (action prompt))
    a <$ embed (atomically (writeTVar menuCursor (newMenu ^. cursor)))

setPrompt ::
  Member (Embed IO) r =>
  MenuState i ->
  Prompt ->
  Sem r ()
setPrompt MenuState {menuPrompt} prompt =
  embed (atomically (writeTVar menuPrompt prompt))

menuWriteSem ::
  Members [Resource, Embed IO] r =>
  MenuSem r i a ->
  MenuStateSem r i a
menuWriteSem action = do
  menuState <- ask
  insertAt @0 (runMenuM menuState (const action))

menuRead ::
  Members [Resource, Embed IO] r =>
  MenuSem r i a ->
  MenuStateSem r i a
menuRead action = do
  menuState <- ask
  insertAt @0 (runMenuMRead menuState (const action))
