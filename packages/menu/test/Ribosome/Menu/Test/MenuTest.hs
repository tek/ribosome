module Ribosome.Menu.Test.MenuTest where

import Control.Concurrent.Lifted (threadDelay)
import Control.Lens (use, view, (^.))
import qualified Control.Monad.Trans.State.Strict as MTL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map (fromList)
import Polysemy (run)
import Polysemy.Conc (interpretAtomic, interpretRace)
import Polysemy.Log (interpretLogNull)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import qualified Streamly.Prelude as Streamly
import Streamly.Prelude (MonadAsync, SerialT)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Menu.Action (menuIgnore, menuQuit)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Consumer as Consumer
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), simpleIntEntries)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.Menu (Menu, consMenu)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig), hoistMenuConfig)
import Ribosome.Menu.Data.MenuConsumer (MenuApp (MenuApp), MenuConsumer, MenuWidgetSem)
import qualified Ribosome.Menu.Data.MenuData as MenuItems
import Ribosome.Menu.Data.MenuData (cursor)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer (MenuRenderer))
import Ribosome.Menu.Data.MenuStateSem (menuRead, semState)
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

sleep ::
  Double ->
  IO ()
sleep t =
  threadDelay (round (t * 1000000))

promptInput ::
  MonadIO m =>
  MonadAsync m =>
  [Text] ->
  SerialT m PromptInputEvent
promptInput chars =
  Streamly.fromListM [PromptInputEvent.Character c <$ liftIO (sleep 0.01) | c <- chars]

menuItems ::
  Monad m =>
  [Text] ->
  SerialT m (MenuItem Text)
menuItems =
  Streamly.fromList . fmap (simpleMenuItem "name")

storePrompt ::
  Members [AtomicState [Prompt], Resource, Embed IO] r =>
  MenuEvent ->
  MenuWidgetSem r Text a
storePrompt = \case
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
        prompt <- semState (use Menu.prompt)
        atomicModify' (prompt :)
        raise menuIgnore

render ::
  MVar [[Entry Text]] ->
  Menu Text ->
  MenuRenderEvent ->
  IO ()
render varItems menu = \case
  MenuRenderEvent.Render -> do
    let cur = view sortedEntries menu
    modifyMVar_ varItems (pure . (cur :))
  MenuRenderEvent.Quit ->
    unit

menuTest ::
  Members [Resource, Race, Embed IO, Final IO] r =>
  MenuConsumer r Text a ->
  [Text] ->
  [Text] ->
  Sem r [[Entry Text]]
menuTest consumer items chars = do
  itemsVar <- embed (newMVar [])
  _ <- interpretLogNull $ runMenu (conf itemsVar)
  embed (readMVar itemsVar)
  where
    conf itemsVar =
      hoistMenuConfig raise raise3Under $
      MenuConfig (menuItems items) fuzzyItemFilter consumer (MenuRenderer (render itemsVar)) promptConfig
    promptConfig =
      PromptConfig (PromptInput (const (promptInput chars))) basicTransition noPromptRenderer [StartInsert]

promptTest ::
  Members [AtomicState [Prompt], Resource, Race, Embed IO, Final IO] r =>
  [Text] ->
  [Text] ->
  Sem r ([[Entry Text]], [Prompt])
promptTest items chars = do
  itemsResult <- menuTest (Consumer.forApp (MenuApp storePrompt)) items chars
  (itemsResult,) <$> atomicGet

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
test_pureMenuModeChange =
  runTestAuto $ interpretRace $ interpretAtomic [] do
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
  runTestAuto $ interpretRace $ interpretAtomic [] do
    items <- fst <$> promptTest items2 chars2
    [itemsTarget] === (fmap (view Entry.item) <$> take 1 items)

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
  Members [AtomicState [Text], Resource, Embed IO] r =>
  MenuWidgetSem r Text a
exec =
  menuRead do
    fs <- semState (use sortedEntries)
    atomicPut (view (Entry.item . MenuItem.text) <$> fs)
    menuQuit

test_pureMenuExecute :: UnitTest
test_pureMenuExecute = do
  runTestAuto $ interpretRace $ interpretAtomic [] do
    _ <- menuTest (Consumer.forMappings (Map.fromList [("cr", exec)])) items3 chars3
    (items3 ===) =<< atomicGet

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
  Members [AtomicState (Maybe (NonEmpty Text)), Resource, Embed IO] r =>
  MenuWidgetSem r Text a
execMulti = do
  menuRead do
    selection <- semState (use selected')
    atomicPut (fmap MenuItem._text <$> selection)
    menuQuit

test_menuMultiMark :: UnitTest
test_menuMultiMark = do
  runTestAuto $ interpretRace $ interpretAtomic Nothing do
    _ <- menuTest (Consumer.withMappings (Map.fromList [("cr", execMulti)])) itemsMulti charsMulti
    (Just ["item5", "item4", "item3"] ===) =<< atomicGet

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
  Members [AtomicState (Maybe (NonEmpty Text)), Resource, Embed IO] r =>
  MenuWidgetSem r Text a
execToggle = do
  menuRead do
    selection <- semState (use selectedOnly)
    atomicPut (fmap MenuItem._text <$> selection)
  menuQuit

test_menuToggle :: UnitTest
test_menuToggle = do
  runTestAuto $ interpretRace $ interpretAtomic Nothing do
    _ <- menuTest (Consumer.withMappings (Map.fromList [("cr", execToggle)])) itemsToggle charsToggle
    (Just ["abc", "a"] ===) =<< atomicGet

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
  Members [AtomicState [Text], Resource, Embed IO] r =>
  MenuWidgetSem r Text a
execExecuteThunk =
  menuRead do
    sel <- semState (use focus)
    Nothing <$ atomicModify' (append sel)
  where
    append sel a =
      a ++ maybeToList (MenuItem._text <$> sel)

test_menuExecuteThunk :: UnitTest
test_menuExecuteThunk = do
  runTestAuto $ interpretRace $ interpretAtomic mempty do
    _ <- menuTest (Consumer.withMappings (Map.fromList [("cr", execExecuteThunk)])) itemsExecuteThunk charsExecuteThunk
    (["a", "b"] ===) =<< atomicGet

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
  runTestAuto do
    targetSel === IntMap.elems (MenuItem._text <$> updatedSel ^. MenuItems.items)
    2 === updatedSel ^. cursor
    targetFoc === IntMap.elems (MenuItem._text <$> updatedFoc ^. MenuItems.items)
    (([0], [9]), 9) === second (length . sortEntries) (popSelection 0 unselectedEntries)
    75000 === length (sortEntries (snd (popSelection manyCursor manyEntries)))
    (([30000], [70000]), 100000) === second (length . sortEntries) (popSelection manyCursor manyUnselectedEntries)
    where
      updatedSel =
        run (execState (menu entriesSel) deleteSelected)
      updatedFoc =
        run (execState (menu entriesFoc) deleteSelected)
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
        cons [(n, Entry (simpleMenuItem () "") n (n >= 50000 && even n)) | n <- [0..100000]]
      manyUnselectedEntries =
        cons [(n, Entry (simpleMenuItem () "") n False) | n <- [0..100000]]
      cons =
        Entry.fromList . reverse
      manyCursor =
        30000

test_menuUnselectedCursor :: UnitTest
test_menuUnselectedCursor =
  runTestAuto do
    [2, 4] === (MenuItem._meta <$> MTL.evalState (use unselected) menu)
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
