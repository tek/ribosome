module Ribosome.Menu.Test.MenuTest where

import qualified Control.Monad.Trans.State.Strict as MTL
import qualified Data.IntMap.Strict as IntMap
import Hedgehog (TestLimit, property, test, withTests)
import Lens.Micro.Mtl (use, view)
import Polysemy (run)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Ribosome.Host.Test.Run (runUnitTest)
import Ribosome.Menu.Action (menuQuit, menuSuccess)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), simpleIntEntries)
import Ribosome.Menu.Data.Menu (consMenu)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.MenuState (MenuWidget)
import Ribosome.Menu.Effect.MenuState (MenuState, viewMenu)
import Ribosome.Menu.Effect.MenuTest (result, sendCharWait, sendStaticItems)
import Ribosome.Menu.ItemLens (items, selected', selectedOnly, unselected)
import Ribosome.Menu.Items (deleteSelected, popSelection)
import Ribosome.Menu.MenuTest (runStaticTestMenu, runTestMenu, testMenu, testStaticMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag (StartInsert))
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert, Normal))
import Ribosome.Menu.Test.Menu (assertItems, assertPrompt, promptTest)

unitTestTimes ::
  TestLimit ->
  TestName ->
  UnitTest ->
  TestTree
unitTestTimes n desc =
  testProperty desc . withTests n . property . test

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
    sendCharWait "i"
    assertPrompt (Prompt 1 Insert "i")
    assertItems ["i1", "i2", "i3", "i4"]
    sendCharWait "esc"
    assertPrompt (Prompt 0 Normal "i")
    assertItems ["i1", "i2", "i3", "i4"]
    sendCharWait "a"
    assertPrompt (Prompt 1 Insert "i")
    assertItems ["i1", "i2", "i3", "i4"]
    sendCharWait "2"
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
    sendCharWait "l"
    assertItems ["long", "long-item", "longitem"]
    sendCharWait "-"
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
  Member (MenuState i) r =>
  MenuWidget r [Text]
exec = do
  fs <- viewMenu sortedEntries
  menuSuccess (view (#item . #text) <$> fs)

test_pureMenuExecute :: UnitTest
test_pureMenuExecute = do
  runUnitTest do
    runTestMenu [] [("cr", exec)] do
      r <- testMenu do
        sendStaticItems (simpleMenuItem () <$> items3)
        sendCharWait "cr"
        result
      Success items3 === r

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
  Member (MenuState i) r =>
  MenuWidget r (Maybe (NonEmpty Text))
execMulti = do
  selection <- viewMenu selected'
  menuSuccess (fmap MenuItem.text <$> selection)

test_menuMultiMark :: UnitTest
test_menuMultiMark = do
  runUnitTest do
    runTestMenu [StartInsert] [("cr", execMulti)] do
      r <- testMenu do
        sendStaticItems (simpleMenuItem () <$> itemsMulti)
        traverse_ sendCharWait charsMulti
        result
      Success (Just ["item5", "item4", "item3"]) === r

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
  Member (MenuState i) r =>
  MenuWidget r (NonEmpty Text)
execToggle = do
  viewMenu selectedOnly >>= maybe menuQuit \ selection ->
    menuSuccess (MenuItem.text <$> selection)

test_menuToggle :: UnitTest
test_menuToggle = do
  runUnitTest do
    runStaticTestMenu (simpleMenuItem () <$> itemsToggle) [StartInsert] [("cr", execToggle)] do
      r <- testStaticMenu do
        traverse_ sendCharWait charsToggle
        result
      Success ["abc", "a"] === r

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
    2 === updatedSel ^. #cursor
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
        consMenu its ent mempty 7 mempty 3
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
      consMenu mempty entries mempty 0 mempty 1
    entries =
      simpleIntEntries [2, 3, 4]

test_menu :: TestTree
test_menu =
  testGroup "basic" [
    unitTestTimes 50 "change mode" test_pureMenuModeChange,
    unitTestTimes 50 "filter items" test_pureMenuFilter,
    unitTestTimes 50 "execute an action" test_pureMenuExecute,
    unitTestTimes 50 "mark multiple items" test_menuMultiMark,
    unitTestTimes 50 "toggle selection items" test_menuToggle,
    unitTest "delete selected" test_menuDeleteSelected,
    unitTest "unselected items with no selected items" test_menuUnselectedCursor
  ]
