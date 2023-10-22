module Ribosome.Menu.Test.CursorClampTest where

import Polysemy.Test (UnitTest, assertEq)

import Ribosome.Api.Buffer (bufferContent)
import qualified Ribosome.Data.ScratchState
import Ribosome.Menu.App (defaultHandlers)
import Ribosome.Menu.Data.Filter (substring)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query), QueryEvent (Refined))
import Ribosome.Menu.Data.State (modal)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMappingRender, sendPrompt, waitEvent)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))
import Ribosome.Menu.Scratch (menuScratch)
import Ribosome.Menu.Test.Run (testStaticNvimMenu)
import Ribosome.Menu.Test.Util (staticMenuItems)
import Ribosome.Test.Embed (testEmbed_)
import Ribosome.Test.Wait (assertWait)

test_clampCursor :: UnitTest
test_clampCursor = do
  testEmbed_ do
    testStaticNvimMenu its def (modal substring) (menuScratch & #maxSize ?~ 10) maps do
      scr <- MenuUi.itemsScratch
      sendMappingRender "j"
      sendPrompt (Prompt 1 Insert "1")
      waitEvent "1" (Query Refined)
      assertWait (bufferContent scr.buffer) (assertEq ["11", "12", "13"])
      MenuTest.quit
  where
    maps = defaultHandlers

    its = staticMenuItems (reverse ["11", "12", "13", "44", "55", "66", "77", "88", "99"])
