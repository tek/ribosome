module Ribosome.Menu.Test.RefineManyTest where

import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Test (UnitTest, assertEq)

import Ribosome.Host.Test.Run (runTest)
import Ribosome.Menu.Data.Filter (Filter (Substring))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query), QueryEvent (Modal, Refined))
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Effect.MenuTest (itemsDone, quit, sendPrompt, waitEvent)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.MenuTest (runTestMenu, testStaticMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))
import Ribosome.Menu.Test.Menu (assertCurrent)
import Ribosome.Test.Error (testError)

test_refineMany :: UnitTest
test_refineMany = do
  runTest do
    runTestMenu startInsert $ defaultFilter do
      testError $ testStaticMenu its def (modal Substring) mempty do
        itemsDone "initial"
        sendPrompt (Prompt 1 Insert "1")
        waitEvent "1" (Query Refined)
        assertLength 100
        sendPrompt (Prompt 2 Insert "12")
        waitEvent "12" (Query Refined)
        assertLength 90
        sendPrompt (Prompt 3 Insert "121")
        sendPrompt (Prompt 4 Insert "1211")
        sendPrompt (Prompt 5 Insert "12111")
        waitEvent "1231" (Query Refined)
        assertLength 0
        sendPrompt (Prompt 2 Insert "12")
        waitEvent "12" (Query Modal)
        assertLength 90
        quit
  where
    assertLength n = assertCurrent \ i -> assertEq (n * factor) (length i)
    its =
      fmap (simpleMenuItem ()) [toText cs | ini <- toList (NonEmpty.inits full), cs <- replicate (10 * factor) ini]
    full = NonEmpty.fromList "1234567890"
    factor = 100
