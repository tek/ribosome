module Ribosome.Menu.Test.DeleteCursorTest where

import Polysemy.Test (UnitTest, assertEq)

import Ribosome.Api.Buffer (bufferContent)
import Ribosome.Api.Window (windowLine)
import qualified Ribosome.Data.ScratchState
import Ribosome.Menu.Action (menuDelete)
import Ribosome.Menu.App (defaultHandlers)
import Ribosome.Menu.Data.Filter (fuzzy)
import Ribosome.Menu.Data.State (modal)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMappingRender)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Scratch (menuScratch)
import Ribosome.Menu.Test.Run (testStaticNvimMenu)
import Ribosome.Menu.Test.Util (staticMenuItems)
import Ribosome.Test.Embed (testEmbed_)

test_deleteCursor :: UnitTest
test_deleteCursor =
  testEmbed_ do
    testStaticNvimMenu its def (modal fuzzy) (menuScratch & #maxSize ?~ 4) maps do
      scr <- MenuUi.itemsScratch
      sendMappingRender "j"
      sendMappingRender "d"
      assertEq 0 =<< windowLine scr.window
      sendMappingRender "d"
      assertEq 0 =<< windowLine scr.window
      assertEq ["3", "4", "5", "6"] =<< bufferContent scr.buffer
      MenuTest.quit
  where
    maps = defaultHandlers <> [("d", menuDelete)]

    its = staticMenuItems (reverse (show <$> [1 :: Int .. 8]))
