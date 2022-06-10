module Ribosome.Menu.Test.MenuTest where

import Conc (Restoration, interpretSyncAs)
import Control.Concurrent.Lifted (threadDelay)
import Control.Lens (use, view, (^.))
import qualified Control.Monad.Trans.State.Strict as MTL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map (fromList)
import Polysemy (run)
import Polysemy.Conc (interpretAtomic, interpretMaskFinal, interpretRace)
import Polysemy.Log (interpretLogNull)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import qualified Streamly.Prelude as Streamly
import Streamly.Prelude (SerialT)
import qualified Sync
import Test.Tasty (TestTree, testGroup)

import Ribosome.Menu.Action (menuIgnore, menuQuit)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), simpleIntEntries)
import Ribosome.Menu.Data.Menu (consMenu)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem, simpleMenuItem)
import Ribosome.Menu.Data.MenuState (
  CursorLock,
  MenuRead,
  MenuStack,
  MenuStateEffects,
  MenuWidget,
  menuRead,
  semState,
  )
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent))
import qualified Ribosome.Menu.Effect.MenuRenderer as MenuRenderer
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, withPrompt)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.Interpreter.MenuConsumer (forMappings, withMappings)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNull)
import Ribosome.Menu.ItemLens (cursor, focus, items, selected', selectedOnly, unselected)
import Ribosome.Menu.Items (deleteSelected, popSelection)
import Ribosome.Menu.Main (interpretMenu, menuMain)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag (StartInsert), PromptListening)
import Ribosome.Menu.Prompt.Input (promptInputWith)

sleep ::
  Double ->
  IO ()
sleep t =
  threadDelay (round (t * 1000000))

menuItems ::
  Monad m =>
  [Text] ->
  SerialT m (MenuItem Text)
menuItems =
  Streamly.fromList . fmap (simpleMenuItem "name")

store ::
  Member (AtomicState [Prompt]) r =>
  Members (MenuStateEffects i) r =>
  Members [Sync CursorLock, Resource, Embed IO] r =>
  MenuWidget r a
store = do
  menuRead do
    prompt <- semState (use #prompt)
    atomicModify' (prompt :)
    raise menuIgnore

storePrompt ::
  Member (AtomicState [Prompt]) r =>
  Members (MenuStateEffects i) r =>
  Members [Sync CursorLock, Resource, Embed IO] r =>
  MenuConsumer () m x ->
  Sem r x
storePrompt = \case
    MenuConsumerEvent MenuEvent.PromptEdit ->
      store
    MenuConsumerEvent (MenuEvent.Mapping _) ->
      store
    MenuConsumerEvent (MenuEvent.Quit _) ->
      menuQuit
    MenuConsumerEvent _ ->
      menuIgnore

render ::
  Members [Sync [[Entry Text]], Mask Restoration, Resource] r =>
  InterpreterFor (MenuRenderer Text) r
render =
  interpret \case
    MenuRenderer.MenuRender menu -> do
      Sync.modify_ (pure . (menu ^. sortedEntries :))
    MenuRenderer.MenuRenderQuit ->
      unit

type TestStack =
  [
    Scoped () PromptRenderer,
    MenuRenderer Text,
    Sync PromptListening
  ] ++ MenuStack Text ++ [
    Log,
    Sync [[Entry Text]],
    Mask Restoration
  ]

runMenuTest ::
  Members [Resource, Race, Embed IO, Final IO] r =>
  InterpretersFor TestStack r
runMenuTest =
  interpretMaskFinal .
  interpretSyncAs mempty .
  interpretLogNull .
  interpretMenu .
  render .
  interpretPromptRendererNull

menuTest ::
  Show a =>
  Members (MenuStack Text) r =>
  Members [MenuConsumer a, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  Members [MenuRenderer Text, Sync PromptListening, Scoped pres PromptRenderer, Sync [[Entry Text]]] r =>
  [Text] ->
  [Text] ->
  Sem r [[Entry Text]]
menuTest its chars =
  withPrompt $ interpretPromptEventsDefault flags do
    menuMain conf *> Sync.block
  where
    conf =
      MenuConfig (menuItems its) (fuzzyItemFilter False) promptConfig
    promptConfig =
      PromptConfig (promptInputWith Nothing (Just 0.01) (Streamly.fromList chars)) flags
    flags =
      [StartInsert]

promptTest ::
  Members [AtomicState [Prompt], Resource, Race, Embed IO, Final IO] r =>
  [Text] ->
  [Text] ->
  Sem r [[Entry Text]]
promptTest its chars =
  runMenuTest $ interpret storePrompt $ interpretPromptRendererNull $ withPrompt do
    menuTest its chars

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
    its <- promptTest items1 chars1
    itemsTarget1 === (fmap Entry.item <$> take 3 its)

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
    its <- promptTest items2 chars2
    [itemsTarget] === (fmap Entry.item <$> take 1 its)

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
  MenuRead i r =>
  Member (AtomicState [Text]) r =>
  MenuWidget r ()
exec =
  menuRead do
    fs <- semState (use sortedEntries)
    atomicPut (view (#item . #text) <$> fs)
    menuQuit

test_pureMenuExecute :: UnitTest
test_pureMenuExecute = do
  runTestAuto $ interpretRace $ interpretAtomic [] do
    runMenuTest $ forMappings [("cr", exec)] do
      void $ menuTest items3 chars3
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
  MenuRead i r =>
  Member (AtomicState (Maybe (NonEmpty Text))) r =>
  MenuWidget r ()
execMulti = do
  menuRead do
    selection <- semState (use selected')
    atomicPut (fmap MenuItem.text <$> selection)
    menuQuit

test_menuMultiMark :: UnitTest
test_menuMultiMark = do
  runTestAuto $ interpretRace $ interpretAtomic Nothing do
    void $ runMenuTest $ withMappings (Map.fromList [("cr", execMulti)]) do
      menuTest itemsMulti charsMulti
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
  MenuRead i r =>
  Member (AtomicState (Maybe (NonEmpty Text))) r =>
  MenuWidget r ()
execToggle = do
  menuRead do
    selection <- semState (use selectedOnly)
    atomicPut (fmap MenuItem.text <$> selection)
  menuQuit

test_menuToggle :: UnitTest
test_menuToggle = do
  runTestAuto $ interpretRace $ interpretAtomic Nothing do
    void $ runMenuTest $ withMappings (Map.fromList [("cr", execToggle)]) do
      menuTest itemsToggle charsToggle
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
  MenuRead i r =>
  Member (AtomicState [Text]) r =>
  MenuWidget r ()
execExecuteThunk =
  menuRead do
    sel <- semState (use focus)
    Nothing <$ atomicModify' (append sel)
  where
    append sel a =
      a ++ maybeToList (MenuItem.text <$> sel)

test_menuExecuteThunk :: UnitTest
test_menuExecuteThunk = do
  runTestAuto $ interpretRace $ interpretAtomic mempty do
    void $ runMenuTest $ withMappings (Map.fromList [("cr", execExecuteThunk)]) do
      menuTest itemsExecuteThunk charsExecuteThunk
    (["a", "b"] ===) =<< atomicGet

testItems :: (Items (), Entries ())
testItems =
  (its, entries)
  where
    its =
      IntMap.fromList ([1..8] <&> \ n -> (n - 1, simpleMenuItem () (show n)))
    entries =
      IntMap.singleton 0 (uncurry newEntry . second (simpleMenuItem ()) <$> [(1, "2"), (3, "4"), (5, "6"), (7, "8")])
    newEntry index item =
      Entry item index False

test_menuDeleteSelected :: UnitTest
test_menuDeleteSelected = do
  runTestAuto do
    targetSel === IntMap.elems (MenuItem.text <$> updatedSel ^. items)
    2 === updatedSel ^. cursor
    targetFoc === IntMap.elems (MenuItem.text <$> updatedFoc ^. items)
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
        consMenu its ent mempty 7 mempty 3 def
      its =
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
    [2, 4] === (MenuItem.meta <$> MTL.evalState (use unselected) menu)
  where
    menu =
      consMenu mempty entries mempty 0 mempty 1 def
    entries =
      simpleIntEntries [2, 3, 4]

test_menu :: TestTree
test_menu =
  testGroup "basic" [
    unitTest "change mode" test_pureMenuModeChange,
    unitTest "filter items" test_pureMenuFilter,
    unitTest "execute an action" test_pureMenuExecute,
    unitTest "mark multiple items" test_menuMultiMark,
    unitTest "toggle selection items" test_menuToggle,
    unitTest "execute a thunk action" test_menuExecuteThunk,
    unitTest "delete selected" test_menuDeleteSelected,
    unitTest "unselected items with no selected items" test_menuUnselectedCursor
  ]
