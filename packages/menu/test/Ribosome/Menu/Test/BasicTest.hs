module Ribosome.Menu.Test.BasicTest where

import qualified Control.Monad.Trans.State.Strict as MTL
import qualified Data.IntMap.Strict as IntMap
import qualified Lens.Micro.Mtl as Lens
import Lens.Micro.Mtl (view)
import Polysemy (run)
import Polysemy.Test (UnitTest, assertEq, runTestAuto, unitTest, unitTestTimes, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Data.Mapping (MappingLhs)
import Ribosome.Host.Test.Run (runTest)
import Ribosome.Menu.Action (MenuWidget, menuChangeMode, menuQuit, menuSuccess)
import Ribosome.Menu.Class.MenuState (MenuState)
import Ribosome.Menu.Combinators (sortEntries, sortedEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entry (Entry), simpleIntEntries)
import Ribosome.Menu.Data.Filter (Filter (Fuzzy, Prefix, Regex, Substring))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query), QueryEvent (Refined, Reset))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.State (Core (Core), Modal (Modal), Primary (Primary), modal)
import Ribosome.Menu.Data.TestMenuConfig (noItemsConf, startInsert)
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))
import Ribosome.Menu.Effect.Menu (menuState)
import Ribosome.Menu.Effect.MenuTest (
  quit,
  result,
  sendMapping,
  sendMappingRender,
  sendPrompt,
  sendPromptEvent,
  sendPromptWait,
  sendStaticItems,
  setPromptWait,
  waitEvent,
  )
import Ribosome.Menu.ItemLens (selected', selectedOnly, unselected)
import Ribosome.Menu.Items (currentEntries, deleteSelected, popSelection)
import Ribosome.Menu.Lens (use)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent (Mapping))
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))
import Ribosome.Menu.Test.Main (headlessTestMenu)
import Ribosome.Menu.Test.Menu (awaitCurrent)
import Ribosome.Menu.Test.RefineManyTest (test_fastPromptAcc, test_refineMany)
import Ribosome.Menu.Test.Run (runTestMenu, testStaticMenu)
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

test_pureFilter :: UnitTest
test_pureFilter = do
  runTest $ runTestMenu $ testError $ headlessTestMenu noItemsConf (modal Fuzzy) [("<cr>", exec)] do
    sendStaticItems "filter initial" (simpleMenuItem () <$> items2)
    assertEq items2 =<< currentEntries
    setPromptWait "l"
    assertEq ["long", "long-item", "longitem"] =<< currentEntries
    setPromptWait "l-"
    assertEq ["long-item"] =<< currentEntries

chars3 :: [Text]
chars3 =
  ["i", "<esc>", "<cr>"]

items3 :: [Text]
items3 =
  [
    "item1",
    "item2"
  ]

exec ::
  MenuState s =>
  MenuWidget s r [Text]
exec =
  menuState do
    fs <- use sortedEntries
    menuSuccess (view (#item . #text) <$> fs)

test_pureExecute :: UnitTest
test_pureExecute = do
  runTest do
    runTestMenu do
      r <- testError $ headlessTestMenu noItemsConf (modal Fuzzy) [("<cr>", exec)] do
        sendStaticItems "exec initial" (simpleMenuItem () <$> items3)
        sendMapping "<cr>"
        result
      Success items3 === r

charsMulti :: [MappingLhs]
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

execMulti ::
  MenuState s =>
  MenuWidget s r (Maybe (NonEmpty Text))
execMulti =
  menuState do
    selection <- use selected'
    menuSuccess (fmap (.text) <$> selection)

test_multiMark :: UnitTest
test_multiMark = do
  runTest do
    runTestMenu do
      r <- testError $ headlessTestMenu noItemsConf (modal Fuzzy) [("<cr>", execMulti)] do
        sendStaticItems "initial" (simpleMenuItem () <$> itemsMulti)
        traverse_ sendMapping charsMulti
        result
      Success (Just ["item5", "item4", "item3"]) === r

itemsChangeFilter :: [Text]
itemsChangeFilter =
  [
    "xaxbx",
    "xabc",
    "xaxBxcx",
    "ab"
  ]

test_initialFilter :: UnitTest
test_initialFilter =
  runTest do
    testError $ testStaticMenu its noItemsConf (modal Substring) mempty do
      sendPrompt (Prompt 1 Insert "abc")
      waitEvent "substring refined" (Query Refined)
      awaitCurrent ["xabc"]
      quit
  where
    its =
      simpleMenuItem () <$> itemsChangeFilter

test_changeFilter :: UnitTest
test_changeFilter =
  runTest do
    testError $ testStaticMenu its def (modal Substring) maps do
      sendPrompt "abc"
      waitEvent "substring refined" (Query Refined)
      awaitCurrent ["xabc"]
      sendPromptEvent False (Mapping "f")
      waitEvent "fuzzy reset" (Query Reset)
      awaitCurrent ["xabc", "xaxBxcx"]
      sendPrompt "ab"
      waitEvent "delete c reset" (Query Reset)
      awaitCurrent ["ab", "xabc", "xaxBxcx", "xaxbx"]
      sendPromptEvent False (Mapping "s")
      waitEvent "substring reset" (Query Reset)
      awaitCurrent ["xabc", "ab"]
      sendPromptEvent False (Mapping "p")
      waitEvent "prefix reset" (Query Reset)
      awaitCurrent ["ab"]
      sendPrompt "b.c?"
      sendPromptEvent False (Mapping "r")
      awaitCurrent ["xaxbx", "xabc", "xaxBxcx"]
      quit
  where
    its =
      simpleMenuItem () <$> itemsChangeFilter
    maps =
      [
        ("f", menuChangeMode Fuzzy),
        ("s", menuChangeMode Substring),
        ("p", menuChangeMode Prefix),
        ("r", menuChangeMode Regex)
      ]

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
  MenuState s =>
  MenuWidget s r (NonEmpty Text)
execToggle =
  menuState do
    use selectedOnly >>= maybe menuQuit \ selection ->
      menuSuccess ((.text) <$> selection)

test_toggle :: UnitTest
test_toggle = do
  runTest do
    runTestMenu do
      r <- testError $ headlessTestMenu (startInsert noItemsConf) (modal Fuzzy) [("<cr>", execToggle)] do
        sendStaticItems "initial" (simpleMenuItem () <$> itemsToggle)
        sendPromptWait "a"
        sendMappingRender "k"
        sendMappingRender "<space>"
        sendMappingRender "*"
        sendMappingRender "<cr>"
        result
      Success ["abc", "a"] === r

test_deleteSelected :: UnitTest
test_deleteSelected = do
  runTestAuto do
    targetSel === IntMap.elems ((.text) <$> updatedSel ^. #state . #core . #primary . #items)
    2 === updatedSel ^. #cursor
    targetFoc === IntMap.elems ((.text) <$> updatedFoc ^. #state . #core . #primary . #items)
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
        WithCursor (Modal (Core (Primary its ent mempty) 7 0) mempty Fuzzy) 3
      its =
        IntMap.fromList [(n, simpleMenuItem () (show n)) | n <- [0..7]]
      entriesSel =
        cons [(n, Entry (simpleMenuItem () (show n)) (fromIntegral n) (n `elem` sels)) | n <- [2..7]]
      entriesFoc =
        cons [(n, Entry (simpleMenuItem () (show n)) (fromIntegral n) False) | n <- [2..7]]
      unselectedEntries =
        cons [(n, Entry (simpleMenuItem () "") (fromIntegral n) False) | n <- [0..9]]
      sels :: [Int]
      sels =
        [2, 3, 6, 7]
      manyEntries =
        cons [(n, Entry (simpleMenuItem () "") (fromIntegral n) (n >= 50000 && even n)) | n <- [0..100000]]
      manyUnselectedEntries =
        cons [(n, Entry (simpleMenuItem () "") (fromIntegral n) False) | n <- [0..100000]]
      cons =
        Entry.fromList . reverse
      manyCursor =
        30000

test_unselectedCursor :: UnitTest
test_unselectedCursor =
  runTestAuto do
    [2, 4] === ((.meta) <$> MTL.evalState (Lens.use unselected) menu)
  where
    menu =
      WithCursor (Modal (Core (Primary mempty entries mempty) 0 0) mempty Fuzzy) 1
    entries =
      simpleIntEntries [2, 3, 4]

test_basic :: TestTree
test_basic =
  testGroup "basic" [
    unitTestTimes 100 "filter items" test_pureFilter,
    unitTestTimes 100 "execute an action" test_pureExecute,
    unitTestTimes 100 "mark multiple items" test_multiMark,
    unitTestTimes 100 "initial filter race" test_initialFilter,
    unitTestTimes 3 "change filter" test_changeFilter,
    unitTestTimes 100 "toggle selection items" test_toggle,
    unitTestTimes 3 "refine large number of items" test_refineMany,
    unitTestTimes 3 "send many prompts in quick succession" test_fastPromptAcc,
    unitTest "delete selected" test_deleteSelected,
    unitTest "unselected items with no selected items" test_unselectedCursor
  ]
