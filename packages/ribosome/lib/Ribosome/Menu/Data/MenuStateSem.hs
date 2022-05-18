module Ribosome.Menu.Data.MenuStateSem where

import Control.Concurrent.Lifted (newMVar)
import Control.Concurrent.STM (
  STM,
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
import Ribosome.Menu.Data.MenuData (dirty)
import Ribosome.Menu.Data.MenuState (MenuState (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data ItemsLock =
  ItemsLock
  deriving stock (Eq, Show)

data CursorLock =
  CursorLock
  deriving stock (Eq, Show)

-- data MenuState i =
--   MenuState {
--     itemsLock :: MVar (),
--     cursorLock :: MVar (),
--     menuItems :: TVar (MenuItems i),
--     menuCursor :: TVar MenuCursor,
--     menuPrompt :: TVar Prompt
--   }

type MenuStateSem r i a =
  Sem (Reader (MenuState i) : Sync ItemsLock : Sync CursorLock : r) a

type MenuSem r i a =
  Sem (State (Menu i) : r) a

newtype SemS s r a =
  SemS { unSemS :: Sem (State s : r) a }
  deriving newtype (Functor, Applicative, Monad)

semState :: SemS s r a -> Sem (State s : r) a
semState =
  unSemS

instance MonadState s (SemS s r) where
  get = SemS get
  put = SemS . put

-- type MenuItemsM m i a =
--   StateT (MenuItems i) m a

newMenuState ::
  Member (Embed IO) r =>
  Sem r (MenuState i)
newMenuState =
  embed do
    itemsLock <- newMVar ()
    cursorLock <- newMVar ()
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

-- modifyMenuCursor ::
--   MonadIO m =>
--   MonadBaseControl IO m =>
--   MenuState i ->
--   (Prompt -> MenuCursor -> MenuItems i -> m (a, MenuCursor)) ->
--   m a
-- modifyMenuCursor menuState@MenuState {menuCursor, cursorLock} f =
--   withMVar cursorLock \ () -> do
--     Menu items cur prompt <- readMenu menuState
--     (result, newCursor) <- f prompt cur items
--     atomically do
--       writeTVar menuCursor newCursor
--     pure result

-- modifyMenuItems ::
--   MonadIO m =>
--   MonadBaseControl IO m =>
--   MenuState i ->
--   (Prompt -> MenuCursor -> MenuItems i -> m (a, MenuItems i)) ->
--   m a
-- modifyMenuItems menuState@MenuState {menuItems, itemsLock} f =
--   withMVar itemsLock \ () -> do
--     Menu items st prompt <- readMenu menuState
--     (result, newItems) <- f prompt st items
--     atomically do
--       writeTVar menuItems newItems
--       modifyTVar menuItems (dirty .~ True)
--     pure result

-- menuItemsStateT ::
--   MonadIO m =>
--   MonadBaseControl IO m =>
--   MenuState i ->
--   (Prompt -> MenuCursor -> MenuItemsM m i a) ->
--   m a
-- menuItemsStateT menuState f =
--   modifyMenuItems menuState \ p s i -> runStateT (f p s) i

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
  MonadIO m =>
  MenuState i ->
  Prompt ->
  m ()
setPrompt MenuState {menuPrompt} prompt =
  liftIO (atomically (writeTVar menuPrompt prompt))

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
