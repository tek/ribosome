module Ribosome.Menu.Test.RefineManyTest where

import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Test (UnitTest)

import Ribosome.Host.Test.Run (runTest)
import Ribosome.Menu.Data.Filter (Filter (Substring))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query), QueryEvent (Modal, Refined))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Effect.MenuTest (itemsDone, quit, sendPrompt, waitEvent)
import Ribosome.Menu.MenuTest (runTestMenu, testStaticMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))
import Ribosome.Menu.Test.Menu (assertItemCount)
import Ribosome.Test.Error (testError)

items :: Int -> [MenuItem ()]
items factor =
  simpleMenuItem () <$> [toText cs | ini <- toList (NonEmpty.inits full), cs <- replicate factor ini]
  where
    full = NonEmpty.fromList "1234567890"

test_refineMany :: UnitTest
test_refineMany = do
  runTest do
    runTestMenu startInsert do
      testError $ testStaticMenu (items factor) def (modal Substring) mempty do
        itemsDone "initial"
        sendPrompt (Prompt 1 Insert "1")
        waitEvent "1" (Query Refined)
        assertCount 10
        sendPrompt (Prompt 2 Insert "12")
        waitEvent "12" (Query Refined)
        assertCount 9
        sendPrompt (Prompt 3 Insert "121")
        sendPrompt (Prompt 4 Insert "1211")
        sendPrompt (Prompt 5 Insert "12111")
        waitEvent "1231" (Query Refined)
        assertCount 0
        sendPrompt (Prompt 2 Insert "12")
        waitEvent "12" (Query Modal)
        assertCount 9
        quit
  where
    assertCount n = assertItemCount (n * factor)
    factor = 1000

test_fastPromptAcc :: UnitTest
test_fastPromptAcc = do
  runTest do
    runTestMenu startInsert do
      testError $ testStaticMenu (items factor) def (modal Substring) mempty do
        itemsDone "initial"
        sendPrompt (Prompt 1 Insert "1")
        sendPrompt (Prompt 2 Insert "12")
        sendPrompt (Prompt 3 Insert "123")
        sendPrompt (Prompt 4 Insert "1234")
        sendPrompt (Prompt 5 Insert "12345")
        sendPrompt (Prompt 6 Insert "123456")
        sendPrompt (Prompt 7 Insert "1234567")
        sendPrompt (Prompt 8 Insert "12345678")
        waitEvent "refined" (Query Refined)
        assertItemCount (3 * factor)
        quit
  where
    factor = 1000
