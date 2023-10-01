module Ribosome.Menu.Test.BottomStatusTest where

import Exon (exon)
import Polysemy.Test (UnitTest, assertEq, (===))

import Ribosome.Api.Buffer (bufferContent)
import Ribosome.Menu.App (defaultHandlers)
import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (MenuMode (cycleFilter, renderExtra, renderFilter))
import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (MenuState (renderStatus))
import Ribosome.Menu.Data.Filter (Filter (Fuzzy), basicMatcher)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.State (Modal, modal)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Test.Run (confSet, testStaticNvimMenu)
import Ribosome.Menu.Prompt.Run (nosync, withPromptInputSync)
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.SocketTmux (testSocketTmux)

data TestMode =
  TestMode {
    mode :: Filter,
    count :: Int
  }
  deriving stock (Eq, Show, Ord, Generic)

data TestState =
  TestState {
    testModal :: Modal TestMode Text,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)

instance MenuMode Text TestMode where

  cycleFilter TestMode {..} =
    TestMode {mode = cycleFilter mode, ..}

  renderFilter TestMode {mode} =
    renderFilter mode

  renderExtra TestMode {count} _ =
    Just [exon|count: #{show count}|]

  matcher TestMode {mode} = basicMatcher mode

instance MenuState TestState where
  type Item TestState = Text
  type Mode TestState = TestMode

  core = #testModal . #core

  mode = #testModal . #mode

  histories = #testModal . #history

  renderStatus (TestState {message}) _ =
    [[exon|message: #{message}|]]

-- TODO withPromptInputSync here is partially ineffective since it sets menuSync, which should be read by the main loop.
test_bottomStatus :: UnitTest
test_bottomStatus =
  testSocketTmux do
    result <- testStaticNvimMenu @() [] conf (TestState (modal (TestMode Fuzzy 0)) "empty") def defaultHandlers do
      status <- MenuUi.statusScratch
      assertEq ["message: empty"] . drop 1 =<< bufferContent (status ^. #buffer)
      awaitScreenshot False "menu-bottom-status" 0
      withPromptInputSync [nosync "<esc>"] do
        MenuTest.result
    MenuResult.Aborted === result
  where
    conf = confSet #nativePrompt True def
