module Ribosome.Test.MenuTest where

import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Lens (use, view)
import Control.Monad.Catch (MonadCatch)
import qualified Control.Monad.Trans.Reader as MTL
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map (fromList)
import Hedgehog ((===))
import qualified Streamly.Prelude as Streamly
import Streamly.Prelude (MonadAsync, SerialT)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Control.StrictRibosome (StrictRibosome)
import Ribosome.Menu.Action (menuIgnore, menuQuit)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Consumer as Consumer
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), simpleIntEntries)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.Menu (Menu, consMenu)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuApp (MenuApp), MenuConsumer, MenuWidget)
import qualified Ribosome.Menu.Data.MenuData as MenuItems
import Ribosome.Menu.Data.MenuData (cursor)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer (MenuRenderer))
import Ribosome.Menu.Data.MenuState (menuRead)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.ItemLens (focus, selected', selectedOnly, unselected)
import Ribosome.Menu.Items (deleteSelected, popSelection)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (
  PromptConfig (PromptConfig),
  PromptFlag (StartInsert),
  PromptInput (PromptInput),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState
import Ribosome.Menu.Prompt.Run (noPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Menu.Run (runMenu)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Run (UnitTest, unitTest)

promptInput ::
  MonadIO m =>
  MonadAsync m =>
  [Text] ->
  SerialT m PromptInputEvent
promptInput chars =
  Streamly.fromListM [PromptInputEvent.Character c <$ sleep 0.01 | c <- chars]

menuItems ::
  Monad m =>
  [Text] ->
  SerialT m (MenuItem Text)
menuItems =
  Streamly.fromList . fmap (simpleMenuItem "name")

storePrompt ::
  MonadIO m =>
  MVar [Prompt] ->
  MenuEvent ->
  MenuWidget m Text a
storePrompt prompts = \case
    MenuEvent.PromptEdit ->
      store
    MenuEvent.Mapping _ ->
      store
    MenuEvent.Quit _ ->
      menuQuit
    _ ->
      menuIgnore
  where
    store = do
      menuRead do
        prompt <- use Menu.prompt
        void $ modifyMVar_ prompts (return . (prompt :))
        menuIgnore

render ::
  MonadIO m =>
  MVar [[Entry Text]] ->
  MenuRenderEvent ->
  ReaderT (Menu Text) m ()
render varItems = \case
  MenuRenderEvent.Render -> do
    cur <- MTL.asks (view sortedEntries)
    modifyMVar_ varItems (pure . (cur :))
  MenuRenderEvent.Quit ->
    unit

type TestM m = StateT (StrictRibosome ()) m

menuTest ::
  MonadIO m =>
  MonadCatch m =>
  MenuConsumer (TestM m) Text a ->
  [Text] ->
  [Text] ->
  m [[Entry Text]]
menuTest handler items chars = do
  itemsVar <- newMVar []
  _ <- evalStateT (runMenu (conf itemsVar)) def
  readMVar itemsVar
  where
    conf itemsVar =
      MenuConfig (menuItems items) fuzzyItemFilter handler (MenuRenderer (render itemsVar)) promptConfig
    promptConfig =
      PromptConfig (PromptInput (const (promptInput chars))) basicTransition noPromptRenderer [StartInsert]

promptTest ::
  MonadIO m =>
  MonadCatch m =>
  [Text] ->
  [Text] ->
  m ([[Entry Text]], [Prompt])
promptTest items chars = do
  prompts <- newMVar []
  itemsResult <- menuTest (Consumer.forApp (MenuApp (storePrompt prompts))) items chars
  (itemsResult,) <$> readMVar prompts

promptsTarget1 :: [Prompt]
promptsTarget1 =
  [
    Prompt 1 PromptState.Insert "i",
    Prompt 0 PromptState.Normal "i",
    Prompt 2 PromptState.Insert "i2"
  ]

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
    item <$> ["i1", "i2", "i3", "i4"],
    item <$> ["i1", "i2", "i3", "i4"]
  ]
  where
    item =
      simpleMenuItem "name"

test_pureMenuModeChange :: UnitTest
test_pureMenuModeChange = do
  (items, prompts) <- promptTest items1 chars1
  itemsTarget1 === (fmap Entry._item <$> take 3 items)
  promptsTarget1 === reverse prompts

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

test_pureMenuFilter :: UnitTest
test_pureMenuFilter = do
  items <- fst <$> promptTest items2 chars2
  [itemsTarget] === (view Entry.item <$$> take 1 items)

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
  MenuWidget m Text a
exec var = do
  menuRead do
    fs <- use sortedEntries
    void $ swapMVar var (view (Entry.item . MenuItem.text) <$> fs)
    menuQuit

test_pureMenuExecute :: UnitTest
test_pureMenuExecute = do
  var <- newMVar []
  _ <- liftIO (menuTest (Consumer.forMappings (Map.fromList [("cr", exec var)])) items3 chars3)
  (items3 ===) =<< readMVar var

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
  MenuWidget m Text a
execMulti var = do
  menuRead do
    selection <- use selected'
    void $ swapMVar var (fmap MenuItem._text <$> selection)
    menuQuit

test_menuMultiMark :: UnitTest
test_menuMultiMark = do
  var <- newMVar Nothing
  _ <- menuTest (Consumer.withMappings (Map.fromList [("cr", execMulti var)])) itemsMulti charsMulti
  (Just ["item5", "item4", "item3"] ===) =<< readMVar var

charsToggle :: [Text]
charsToggle =
  ["a", "esc", "k", "space", "*", "cr"]

itemsToggle :: [Text]
itemsToggle =
  [
    "xxx",
    "a",
    "xxx",
    "xxx",
    "ab",
    "xxx",
    "abc",
    "xxx"
  ]

execToggle ::
  MonadIO m =>
  MVar (Maybe (NonEmpty Text)) ->
  MenuWidget m Text a
execToggle var = do
  menuRead do
    selection <- use selectedOnly
    void $ swapMVar var (fmap MenuItem._text <$> selection)
  menuQuit

test_menuToggle :: UnitTest
test_menuToggle = do
  var <- newMVar Nothing
  _ <- menuTest (Consumer.withMappings (Map.fromList [("cr", execToggle var)])) itemsToggle charsToggle
  (Just ["abc", "a"] ===) =<< readMVar var

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
  MVar [Text] ->
  MenuWidget m Text a
execExecuteThunk var =
  menuRead do
    sel <- use focus
    Nothing <$ modifyMVar_ var (append sel)
  where
    append sel a =
      pure (a ++ maybeToList (MenuItem._text <$> sel))

test_menuExecuteThunk :: UnitTest
test_menuExecuteThunk = do
  var <- newMVar []
  _ <- menuTest (Consumer.withMappings (Map.fromList [("cr", execExecuteThunk var)])) itemsExecuteThunk charsExecuteThunk
  (["a", "b"] ===) =<< readMVar var

testItems :: (Items (), Entries ())
testItems =
  (items, entries)
  where
    items =
      IntMap.fromList ([1..8] <&> \ n -> (n - 1, simpleMenuItem () (show n)))
    entries =
      IntMap.singleton 0 (uncurry newEntry . second (simpleMenuItem ()) <$> [(1, "2"), (3, "4"), (5, "6"), (7, "8")])
    newEntry index item =
      Entry item index False

test_menuDeleteSelected :: UnitTest
test_menuDeleteSelected = do
  targetSel === IntMap.elems (MenuItem._text <$> updatedSel ^. MenuItems.items)
  2 === updatedSel ^. cursor
  targetFoc === IntMap.elems (MenuItem._text <$> updatedFoc ^. MenuItems.items)
  (([0], [9]), 9) === second (length . sortEntries) (popSelection 0 unselectedEntries)
  75000 === length (sortEntries (snd (popSelection manyCursor manyEntries)))
  (([30000], [70000]), 100000) === second (length . sortEntries) (popSelection manyCursor manyUnselectedEntries)
  where
    updatedSel =
      execState deleteSelected (menu entriesSel)
    updatedFoc =
      execState deleteSelected (menu entriesFoc)
    targetSel =
      ["0", "1", "4", "5"]
    -- the cursor is counted starting at the highest score, so it removes `4`, not `5`
    targetFoc =
      ["0", "1", "2", "3", "5", "6", "7"]
    menu ent =
      consMenu items ent mempty 7 mempty True 3 def
    items =
      IntMap.fromList [(n, simpleMenuItem () (show n)) | n <- [0..7]]
    entriesSel =
      cons [(n, Entry (simpleMenuItem () (show n)) n (elem n sels)) | n <- [2..7]]
    entriesFoc =
      cons [(n, Entry (simpleMenuItem () (show n)) n False) | n <- [2..7]]
    unselectedEntries =
      cons [(n, Entry (simpleMenuItem () "") n False) | n <- [0..9]]
    sels :: [Int]
    sels =
      [2, 3, 6, 7]
    manyEntries =
      cons [(n, Entry (simpleMenuItem () "") n (n >= 50000 && n `mod` 2 == 0)) | n <- [0..100000]]
    manyUnselectedEntries =
      cons [(n, Entry (simpleMenuItem () "") n False) | n <- [0..100000]]
    cons =
      Entry.fromList . reverse
    manyCursor =
      30000

test_menuUnselectedCursor :: UnitTest
test_menuUnselectedCursor =
  [2, 4] === (MenuItem._meta <$> evalState (use unselected) menu)
  where
    menu =
      consMenu mempty entries mempty 0 mempty True 1 def
    entries =
      simpleIntEntries [2, 3, 4]

test_menu :: TestTree
test_menu =
  testGroup "menu" [
    unitTest "change mode" test_pureMenuModeChange,
    unitTest "filter items" test_pureMenuFilter,
    unitTest "execute an action" test_pureMenuExecute,
    unitTest "mark multiple items" test_menuMultiMark,
    unitTest "toggle selection items" test_menuToggle,
    unitTest "execute a thunk action" test_menuExecuteThunk,
    unitTest "delete selected" test_menuDeleteSelected,
    unitTest "unselected items with no selected items" test_menuUnselectedCursor
  ]
