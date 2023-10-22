module Ribosome.Menu.Test.FilterTest where

import Polysemy.Test (UnitTest, assertEq, runTestAuto, (===))
import qualified Streamly.Prelude as Stream
import Zeugma (runTest)

import Ribosome.Menu.Combinators (sortEntriesText)
import Ribosome.Menu.Data.Filter (fuzzy)
import Ribosome.Menu.Data.MenuItem (Items, simpleItems, simpleMenuItem)
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial), menuFilter)
import Ribosome.Menu.Interpreter.Menu (interpretMenuDeps, interpretMenus)
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)
import Ribosome.Menu.Interpreter.MenuUi (interpretMenuUiNvimNull)
import Ribosome.Menu.Items (currentEntries, currentEntriesText)
import Ribosome.Menu.Loop (menuParams, withUi)
import Ribosome.Test.Error (testError)
import Ribosome.Test.Wait (assertWait)

items :: Items ()
items =
  simpleItems [
    "xaxbx",
    "xabc",
    "xaxbxcx",
    "ab"
  ]

test_filterFuzzy :: UnitTest
test_filterFuzzy =
  runTestAuto do
    r <- interpretFilter (menuFilter fuzzy "ab" (Initial items))
    ["ab", "xabc", "xaxbx", "xaxbxcx"] === sortEntriesText r

test_filterNoSort :: UnitTest
test_filterNoSort =
  runTest do
    interpretFilter $ interpretMenuUiNvimNull $ interpretMenuDeps $ interpretMenus do
      testError $ withUi () $ menuParams (Stream.fromList noSortItems) (modal fuzzy) do
        assertWait currentEntries (assertEq (length noSortItems) . length)
        dbgs =<< currentEntriesText
        unit
  where
    noSortItems = item <$> [1 :: Int .. 120]

    item n = simpleMenuItem n (show n)
