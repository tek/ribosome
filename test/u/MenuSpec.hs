{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yield, yieldMany)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Lens (view)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Map.Strict as Map (fromList)
import Test.Framework

import Ribosome.Control.StrictRibosome (StrictRibosome)
import Ribosome.Menu.Action (menuContinue, menuExecute, menuQuit)
import Ribosome.Menu.Data.FilteredMenuItem (FilteredMenuItem(FilteredMenuItem))
import qualified Ribosome.Menu.Data.FilteredMenuItem as FilteredMenuItem (item)
import Ribosome.Menu.Data.Menu (Menu(Menu), MenuFilter(MenuFilter))
import qualified Ribosome.Menu.Data.Menu as Menu (items)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer(MenuConsumer))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem), simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text), text)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig), PromptFlag(StartInsert))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Ribosome.Menu.Run (runMenu)
import Ribosome.Menu.Simple (
  basicMenu,
  defaultMenu,
  deleteByFilteredIndex,
  fuzzyMenuItemMatcher,
  markedMenuItems,
  markedMenuItemsOnly,
  selectedMenuItem,
  simpleMenu,
  )
import Ribosome.System.Time (sleep)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars = do
  lift $ sleep 0.1
  yieldMany (PromptEvent.Character <$> chars)

menuItems ::
  Monad m =>
  [Text] ->
  ConduitT () [MenuItem Text] m ()
menuItems =
  yield . fmap (simpleMenuItem "name")

storePrompt ::
  MonadBaseControl IO m =>
  MVar [Prompt] ->
  MenuUpdate m a Text ->
  m (MenuConsumerAction m a, Menu Text)
storePrompt prompts (MenuUpdate event menu) =
  check event
  where
    check (MenuEvent.PromptChange prompt) =
      store prompt
    check (MenuEvent.Mapping _ prompt) =
      store prompt
    check (MenuEvent.Quit _) =
      menuQuit menu
    check _ =
      menuContinue menu
    store prompt =
      modifyMVar_ prompts (return . (prompt :)) *> menuContinue menu

render ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MVar [[FilteredMenuItem Text]] ->
  MenuRenderEvent m a Text ->
  m ()
render varItems (MenuRenderEvent.Render _ (Menu _ items _ _ _ _)) = do
  modifyMVar_ varItems (return . (items :))
  sleep 0.01
render _ (MenuRenderEvent.Quit _) =
  return ()

type TestM = StateT (StrictRibosome ()) (ResourceT IO)

menuTest ::
  (MenuUpdate TestM a Text -> TestM (MenuAction TestM a, Menu Text)) ->
  [Text] ->
  [Text] ->
  IO [[FilteredMenuItem Text]]
menuTest handler items chars = do
  itemsVar <- newMVar []
  void $ runResourceT $ runMenu (conf itemsVar) `execStateT` def
  readMVar itemsVar
  where
    conf itemsVar =
      MenuConfig (menuItems items) (MenuConsumer handler) (render itemsVar) promptConfig def
    promptConfig =
      PromptConfig (promptInput chars) basicTransition noPromptRenderer [StartInsert]

promptTest :: [Text] -> [Text] -> IO ([[FilteredMenuItem Text]], [Prompt])
promptTest items chars = do
  prompts <- newMVar []
  itemsResult <- menuTest (basicMenu fuzzyMenuItemMatcher (storePrompt prompts)) items chars
  (itemsResult,) <$> readMVar prompts

promptsTarget1 :: [Prompt]
promptsTarget1 =
  uncurry one' <$> [
    (1, PromptState.Insert),
    (0, PromptState.Normal),
    (0, PromptState.Normal),
    (1, PromptState.Insert)
    ]
  where
    one' c s = Prompt c s "i"

items1 :: [Text]
items1 =
  [
    "i1",
    "j1",
    "i2",
    "i3",
    "j2",
    "i4"
    ]

chars1 :: [Text]
chars1 =
  [
    "i",
    "esc",
    "k",
    "a",
    "2"
    ]

itemsTarget1 :: [[MenuItem Text]]
itemsTarget1 =
  [
    item <$> ["i2"],
    item <$> ["i1", "i2", "i3", "i4"]
    ]
  where
    item =
      simpleMenuItem "name"

test_strictMenuModeChange :: IO ()
test_strictMenuModeChange = do
  (items, prompts) <- promptTest items1 chars1
  assertEqual itemsTarget1 (view FilteredMenuItem.item <$$> take 2 items)
  assertEqual promptsTarget1 (take 4 $ reverse prompts)

chars2 :: [Text]
chars2 =
  ["l", "o", "n", "g", "-", "i", "t", "e", "m"]

items2 :: [Text]
items2 =
  [
    "long",
    "short",
    "long-item",
    "longitem"
    ]

itemsTarget :: [MenuItem Text]
itemsTarget =
  [simpleMenuItem "name" "long-item"]

test_strictMenuFilter :: IO ()
test_strictMenuFilter = do
  items <- fst <$> promptTest items2 chars2
  assertEqual [itemsTarget] (view FilteredMenuItem.item <$$> take 1 items)

chars3 :: [Text]
chars3 =
  ["i", "esc", "cr"]

items3 :: [Text]
items3 =
  [
    "item1",
    "item2"
    ]

exec ::
  MonadIO m =>
  MVar [Text] ->
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m a, Menu Text)
exec var m@(Menu _ items _ _ _ _) _ =
  swapMVar var (view (FilteredMenuItem.item . MenuItem.text) <$> items) *> menuQuit m

test_strictMenuExecute :: IO ()
test_strictMenuExecute = do
  var <- newMVar []
  _ <- menuTest (simpleMenu (Map.fromList [("cr", exec var)])) items3 chars3
  assertEqual items3 =<< readMVar var

charsMulti :: [Text]
charsMulti =
  ["esc", "k", "k", "space", "space", "space", "space", "j", "space", "cr"]

itemsMulti :: [Text]
itemsMulti =
  [
    "item1",
    "item2",
    "item3",
    "item4",
    "item5",
    "item6"
    ]

execMulti ::
  MonadIO m =>
  MVar (Maybe (NonEmpty Text)) ->
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m a, Menu Text)
execMulti var m _ =
  swapMVar var (MenuItem._text <$$> markedMenuItems m) *> menuQuit m

test_menuMultiMark :: IO ()
test_menuMultiMark = do
  var <- newMVar Nothing
  _ <- menuTest (defaultMenu (Map.fromList [("cr", execMulti var)])) itemsMulti charsMulti
  assertEqual (Just ("item3" :| ["item4", "item5"])) =<< readMVar var

charsToggle :: [Text]
charsToggle =
  ["esc", "k", "space", "*", "i", "a", "b", "cr"]

itemsToggle :: [Text]
itemsToggle =
  [
    "a",
    "ab",
    "abc"
    ]

execToggle ::
  MonadIO m =>
  MVar (Maybe (NonEmpty Text)) ->
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m a, Menu Text)
execToggle var m _ =
  swapMVar var (MenuItem._text <$$> markedMenuItemsOnly m) *> menuQuit m

test_menuToggle :: IO ()
test_menuToggle = do
  var <- newMVar Nothing
  _ <- menuTest (defaultMenu (Map.fromList [("cr", execToggle var)])) itemsToggle charsToggle
  assertEqual Nothing =<< readMVar var

charsExecuteThunk :: [Text]
charsExecuteThunk =
  ["esc", "cr", "k", "cr", "esc"]

itemsExecuteThunk :: [Text]
itemsExecuteThunk =
  [
    "a",
    "b"
    ]

execExecuteThunk ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MVar [Text] ->
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m a, Menu Text)
execExecuteThunk var m _ =
  menuExecute (modifyMVar_ var prepend) m
  where
    prepend a =
      pure $ a ++ maybeToList (MenuItem._text <$> selectedMenuItem m)

test_menuExecuteThunk :: IO ()
test_menuExecuteThunk = do
  var <- newMVar []
  _ <- menuTest (defaultMenu (Map.fromList [("cr", execExecuteThunk var)])) itemsExecuteThunk charsExecuteThunk
  assertEqual ["a", "b"] =<< readMVar var

test_menuDeleteByFilteredIndex :: IO ()
test_menuDeleteByFilteredIndex =
  assertEqual target . fmap (view MenuItem.text) . view Menu.items . deleteByFilteredIndex [1, 2] $ menu
  where
    target =
      ["1", "2", "3", "5", "7", "8"]
    menu =
      Menu items filtered 0 [] (MenuFilter "") Nothing
    items =
      simpleMenuItem () <$> ["1", "2", "3", "4", "5", "6", "7", "8"]
    filtered =
      uncurry FilteredMenuItem . second menuItem <$> [(1, "2"), (3, "4"), (5, "6"), (7, "8")]
    menuItem t =
      MenuItem () t t
