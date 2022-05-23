module Ribosome.Menu.Data.MenuState where

-- import Control.Concurrent.Lifted (newMVar, withMVar)
-- import Control.Concurrent.STM (STM, TVar)
-- import Control.Concurrent.STM.Lifted (atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
-- import Control.Lens ((.~), (^.))
-- import Control.Monad.Trans.Control (MonadBaseControl)
-- import Control.Monad.Trans.Reader (ReaderT (ReaderT))
-- import Control.Monad.Trans.State.Strict (StateT (runStateT))
-- import Prelude hiding (newMVar, withMVar)

-- import Ribosome.Menu.Data.Menu (Menu (Menu), cursor)
-- import Ribosome.Menu.Data.MenuData (MenuCursor, MenuItems, dirty)
-- import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

-- data MenuState i =
--   MenuState {
--     itemsLock :: MVar (),
--     cursorLock :: MVar (),
--     menuItems :: TVar (MenuItems i),
--     menuCursor :: TVar MenuCursor,
--     menuPrompt :: TVar Prompt
--   }

-- type MenuStateM m i a =
--   ReaderT (MenuState i) m a

-- type MenuM m i a =
--   StateT (Menu i) m a

-- type MenuItemsM m i a =
--   StateT (MenuItems i) m a

-- newMenuState ::
--   MonadIO m =>
--   m (MenuState i)
-- newMenuState =
--   liftIO do
--     itemsLock <- newMVar ()
--     cursorLock <- newMVar ()
--     menuItems <- newTVarIO def
--     menuCursor <- newTVarIO def
--     menuPrompt <- newTVarIO def
--     pure MenuState {..}

-- readMenuSTM ::
--   MenuState i ->
--   STM (Menu i)
-- readMenuSTM MenuState {..} =
--   Menu <$> readTVar menuItems <*> readTVar menuCursor <*> readTVar menuPrompt

-- readMenu ::
--   MonadIO m =>
--   MenuState i ->
--   m (Menu i)
-- readMenu =
--   liftIO . atomically . readMenuSTM

-- readMenuForRender ::
--   MonadIO m =>
--   MenuState i ->
--   m (Menu i)
-- readMenuForRender menuState@MenuState {..} =
--   liftIO $ atomically do
--     m <- readMenuSTM menuState
--     modifyTVar menuItems (dirty .~ False)
--     pure m

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

-- runMenuM ::
--   MonadIO m =>
--   MonadBaseControl IO m =>
--   MenuState i ->
--   (Prompt -> MenuM m i a) ->
--   m a
-- runMenuM menuState@MenuState {..} f =
--   withMVar itemsLock \ () ->
--     withMVar cursorLock \ () -> do
--       menu <- readMenu menuState
--       prompt <- readTVarIO menuPrompt
--       (result, Menu newItems newState _) <- runStateT (f prompt) menu
--       atomically do
--         writeTVar menuItems newItems
--         writeTVar menuCursor newState
--         modifyTVar menuItems (dirty .~ True)
--       pure result

-- -- runMenuMRead ::
-- --   MonadIO m =>
-- --   MonadBaseControl IO m =>
-- --   MenuState i ->
-- --   (Prompt -> MenuM m i a) ->
-- --   m a
-- -- runMenuMRead menuState@MenuState {..} action =
-- --   withMVar cursorLock \ () -> do
-- --     menu <- readMenu menuState
-- --     prompt <- readTVarIO menuPrompt
-- --     (a, newMenu) <- runStateT (action prompt) menu
-- --     a <$ atomically (writeTVar menuCursor (newMenu ^. cursor))

-- -- setPrompt ::
-- --   MonadIO m =>
-- --   MenuState i ->
-- --   Prompt ->
-- --   m ()
-- -- setPrompt MenuState {menuPrompt} prompt =
-- --   liftIO (atomically (writeTVar menuPrompt prompt))

-- -- menuWrite ::
-- --   MonadIO m =>
-- --   MonadBaseControl IO m =>
-- --   MenuM m i a ->
-- --   MenuStateM m i a
-- -- menuWrite action =
-- --   ReaderT \ menuState ->
-- --     runMenuM menuState (const action)

-- -- menuRead ::
-- --   MonadIO m =>
-- --   MonadBaseControl IO m =>
-- --   MenuM m i a ->
-- --   MenuStateM m i a
-- -- menuRead action = do
-- --   ReaderT \ menuState ->
-- --     runMenuMRead menuState (const action)
