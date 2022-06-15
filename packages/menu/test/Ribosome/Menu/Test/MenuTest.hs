module Ribosome.Menu.Test.MenuTest where

import Conc (interpretSync)
import Control.Lens (use, view, (^.))
import qualified Control.Monad.Trans.State.Strict as MTL
import qualified Data.IntMap.Strict as IntMap
import Polysemy (run)
import Polysemy.Test (UnitTest, assertJust, runTestAuto, unitTest, (===))
import qualified Sync
import Test.Tasty (TestTree, testGroup)
import Time (Seconds (Seconds))

import Ribosome.Host.Test.Run (runUnitTest)
import Ribosome.Menu.Action (menuQuit)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), simpleIntEntries)
import Ribosome.Menu.Data.Menu (consMenu)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, simpleMenuItem)
import Ribosome.Menu.Data.MenuState (MenuRead, MenuWidget, menuRead, semState)
import Ribosome.Menu.Effect.MenuTest (sendChar, sendStaticItems)
import Ribosome.Menu.Interpreter.MenuConsumer (forMappings, withMappings)
import Ribosome.Menu.ItemLens (cursor, items, selected', selectedOnly, unselected)
import Ribosome.Menu.Items (deleteSelected, popSelection)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert, Normal))
import Ribosome.Menu.Test.Menu (assertItems, assertPrompt, menuTestDef, promptTest, runMenuTestStack)

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

test_pureMenuModeChange :: UnitTest
test_pureMenuModeChange =
  runUnitTest $ promptTest @() do
    sendStaticItems (simpleMenuItem () <$> items1)
    assertItems items1
    sendChar "i"
    assertPrompt (Prompt 1 Insert "i")
    assertItems ["i1", "i2", "i3", "i4"]
    sendChar "esc"
    assertPrompt (Prompt 0 Normal "i")
    assertItems ["i1", "i2", "i3", "i4"]
    sendChar "a"
    assertPrompt (Prompt 1 Insert "i")
    assertItems ["i1", "i2", "i3", "i4"]
    sendChar "2"
    assertPrompt (Prompt 2 Insert "i2")
    assertItems ["i2"]

items2 :: [Text]
items2 =
  [
    "long",
    "short",
    "long-item",
    "longitem"
    ]

test_pureMenuFilter :: UnitTest
test_pureMenuFilter = do
  runUnitTest $ promptTest @() do
    sendStaticItems (simpleMenuItem () <$> items2)
    assertItems items2
    sendChar "l"
    assertItems ["long", "long-item", "longitem"]
    sendChar "-"
    assertItems ["long-item"]

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
  Member (Sync [Text]) r =>
  MenuWidget r ()
exec =
  menuRead do
    fs <- semState (use sortedEntries)
    void (Sync.putWait (Seconds 5) (view (#item . #text) <$> fs))
    menuQuit

test_pureMenuExecute :: UnitTest
test_pureMenuExecute = do
  runUnitTest $ interpretSync do
    runMenuTestStack $ forMappings [("cr", exec)] do
      menuTestDef do
        sendStaticItems (simpleMenuItem () <$> items3)
        sendChar "cr"
        assertJust items3 =<< Sync.wait (Seconds 5)

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
  Member (Sync (Maybe (NonEmpty Text))) r =>
  MenuWidget r ()
execMulti =
  menuRead do
    selection <- semState (use selected')
    void (Sync.putWait (Seconds 5) (fmap MenuItem.text <$> selection))
    menuQuit

test_menuMultiMark :: UnitTest
test_menuMultiMark = do
  runUnitTest $ interpretSync do
    runMenuTestStack $ withMappings [("cr", execMulti)] do
      menuTestDef do
        sendStaticItems (simpleMenuItem () <$> itemsMulti)
        traverse_ sendChar charsMulti
        assertJust ["item5", "item4", "item3"] . join =<< Sync.wait (Seconds 5)

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
  Member (Sync (NonEmpty Text)) r =>
  MenuWidget r ()
execToggle = do
  menuRead do
    semState (use selectedOnly) >>= traverse_ \ selection ->
      Sync.putWait (Seconds 5) (MenuItem.text <$> selection)
  menuQuit

test_menuToggle :: UnitTest
test_menuToggle = do
  runUnitTest $ interpretSync do
    runMenuTestStack $ withMappings [("cr", execToggle)] do
      menuTestDef do
        sendStaticItems (simpleMenuItem () <$> itemsToggle)
        traverse_ sendChar charsToggle
        assertJust ["abc", "a"] =<< Sync.wait @(NonEmpty _) (Seconds 5)

charsExecuteThunk :: [Text]
charsExecuteThunk =
  ["esc", "cr", "k", "cr", "esc"]

itemsExecuteThunk :: [Text]
itemsExecuteThunk =
  [
    "a",
    "b"
  ]

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
        cons [(n, Entry (simpleMenuItem () (show n)) n (n `elem` sels)) | n <- [2..7]]
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
    unitTest "delete selected" test_menuDeleteSelected,
    unitTest "unselected items with no selected items" test_menuUnselectedCursor
  ]
