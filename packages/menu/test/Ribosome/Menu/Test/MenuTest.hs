module Ribosome.Menu.Test.MenuTest where

import qualified Control.Monad.Trans.State.Strict as MTL
import qualified Data.IntMap.Strict as IntMap
import qualified Lens.Micro.Mtl as Lens
import Lens.Micro.Mtl (view)
import Polysemy (run)
import Polysemy.Test (UnitTest, assertEq, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Host.Test.Run (runTest, runUnitTest)
import Ribosome.Menu (MenuEvent (Query), QueryEvent (Refined, Reset))
import Ribosome.Menu.Action (MenuWidget, menuChangeFilter, menuQuit, menuSuccess)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), simpleIntEntries)
import Ribosome.Menu.Data.Filter (Filter (Substring), fuzzyMono)
import Ribosome.Menu.Data.Menu (consMenu)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Effect.MenuLoop (readItems)
import Ribosome.Menu.Effect.MenuState (menuState)
import Ribosome.Menu.Effect.MenuTest (quit, result, sendMappingWait, sendPrompt, sendPromptEvent, sendStaticItems, waitEvent, sendMapping)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.ItemLens (items, selected', selectedOnly, unselected)
import Ribosome.Menu.Items (deleteSelected, popSelection)
import Ribosome.Menu.Lens (use)
import Ribosome.Menu.Mappings (defaultMappings)
import Ribosome.Menu.MenuTest (runTestMenu, testMenu, testStaticMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent (Mapping, Update))
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))
import Ribosome.Menu.Test.Menu (assertItems, promptTest, setPrompt)
import Ribosome.Menu.Test.Run (unitTestTimes)
import Ribosome.Test.Error (testError)

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
  runTest $ promptTest @() do
    sendStaticItems "filter initial" (simpleMenuItem () <$> items2)
    assertItems items2
    setPrompt "1" "l"
    assertItems ["long", "long-item", "longitem"]
    setPrompt "1" "l-"
    assertItems ["long-item"]

chars3 :: [Text]
chars3 =
  ["i", "<esc>", "<cr>"]

items3 :: [Text]
items3 =
  [
    "item1",
    "item2"
    ]

exec :: MenuWidget Filter i r [Text]
exec =
  menuState do
    fs <- use (#items . sortedEntries)
    menuSuccess (view (#item . #text) <$> fs)

test_pureMenuExecute :: UnitTest
test_pureMenuExecute = do
  runUnitTest do
    runTestMenu def $ defaultFilter do
      r <- testError $ testMenu def fuzzyMono [("<cr>", exec)] do
        sendStaticItems "exec initial" (simpleMenuItem () <$> items3)
        sendMappingWait "<cr>"
        result
      Success items3 === r

charsMulti :: [Text]
charsMulti =
  ["k", "k", "<space>", "<space>", "<space>", "<space>", "j", "<space>", "<cr>"]

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

execMulti :: MenuWidget Filter i r (Maybe (NonEmpty Text))
execMulti =
  menuState do
    selection <- use selected'
    menuSuccess (fmap MenuItem.text <$> selection)

test_menuMultiMark :: UnitTest
test_menuMultiMark = do
  runTest do
    runTestMenu startInsert $ defaultFilter do
      r <- testError $ testMenu def fuzzyMono (defaultMappings <> [("<cr>", execMulti)]) do
        sendStaticItems "initial" (simpleMenuItem () <$> itemsMulti)
        traverse_ sendMapping charsMulti
        result
      Success (Just ["item5", "item4", "item3"]) === r

itemsChangeFilter :: [Text]
itemsChangeFilter =
  [
    "xaxbx",
    "abc",
    "xaxbxcx",
    "ab"
  ]

test_changeFilter :: UnitTest
test_changeFilter =
  runTest do
    runTestMenu startInsert $ defaultFilter do
      testError $ testStaticMenu its def Substring maps do
        waitEvent "initial refined" (Query Refined)
        sendPrompt (Prompt 1 Insert "abc")
        waitEvent "substring refined" (Query Refined)
        assertCurrent ["abc"]
        sendPromptEvent False (Mapping "f")
        waitEvent "fuzzy reset" (Query Reset)
        assertCurrent ["abc", "xaxbxcx"]
        sendPrompt (Prompt 1 Insert "ab")
        waitEvent "delete c reset" (Query Reset)
        assertCurrent itemsChangeFilter
        sendPromptEvent False (Mapping "s")
        waitEvent "substring reset" (Query Reset)
        assertCurrent ["abc", "ab"]
        quit
  where
    assertCurrent target =
      assertEq target =<< current
    current =
      toListOf (sortedEntries . each . #item . #text) <$> readItems
    its =
      simpleMenuItem () <$> itemsChangeFilter
    maps =
      defaultMappings <> [("f", menuChangeFilter fuzzyMono), ("s", menuChangeFilter Substring)]

charsToggle :: [PromptEvent]
charsToggle =
  Update "a" : (Mapping <$> ["<esc>", "k", "<space>", "*", "<cr>"])

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
  MenuWidget Filter i r (NonEmpty Text)
execToggle =
  menuState do
    use selectedOnly >>= maybe menuQuit \ selection ->
      menuSuccess (MenuItem.text <$> selection)

test_menuToggle :: UnitTest
test_menuToggle = do
  runUnitTest do
    runTestMenu startInsert $ defaultFilter do
      r <- testError $ testMenu startInsert fuzzyMono (defaultMappings <> [("<cr>", execToggle)]) do
        sendStaticItems "initial" (simpleMenuItem () <$> itemsToggle)
        traverse_ (sendPromptEvent True) charsToggle
        result
      Success ["abc", "a"] === r

charsExecuteThunk :: [Text]
charsExecuteThunk =
  ["<esc>", "<cr>", "k", "<cr>", "<esc>"]

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
        consMenu its ent mempty 7 mempty fuzzyMono 3
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
    [2, 4] === (MenuItem.meta <$> MTL.evalState (Lens.use unselected) menu)
  where
    menu =
      consMenu mempty entries mempty 0 mempty fuzzyMono 1
    entries =
      simpleIntEntries [2, 3, 4]

test_menu :: TestTree
test_menu =
  testGroup "basic" [
    unitTestTimes 100 "filter items" test_pureMenuFilter,
    unitTestTimes 100 "execute an action" test_pureMenuExecute,
    unitTestTimes 100 "mark multiple items" test_menuMultiMark,
    unitTest "change filter" test_changeFilter,
    -- TODO flaky
    -- unitTestTimes 100 "toggle selection items" test_menuToggle,
    unitTest "delete selected" test_menuDeleteSelected,
    unitTest "unselected items with no selected items" test_menuUnselectedCursor
  ]
