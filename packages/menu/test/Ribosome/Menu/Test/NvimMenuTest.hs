module Ribosome.Menu.Test.NvimMenuTest where

import Exon (exon)
import Lens.Micro.Mtl (view)
import Polysemy.Test (UnitTest, assertEq, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Host.Api.Data (bufferGetName, vimGetBuffers, vimGetWindows)
import Ribosome.Menu.Action (MenuWidget, menuOk, menuSuccess)
import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (MenuMode (cycleFilter, renderExtra, renderFilter))
import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (MenuState (renderStatus), entries)
import Ribosome.Menu.Combinators (sortEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Filter (Filter (Fuzzy), basicMatcher)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.State (Modal, modal)
import Ribosome.Menu.Effect.Menu (readCursor, readState)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMapping, sendMappingPrompt, waitEvent)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Interpreter.Menu (interpretSingleWindowMenu, promptInput)
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)
import Ribosome.Menu.Loop (menuMaps, runMenuUi, windowMenu, withMenuUi)
import Ribosome.Menu.Mappings (Mappings, defaultMappings)
import Ribosome.Menu.MenuTest (confSet, testStaticNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (onlyInsert, startInsert)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent (Mapping, Update))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Run (SyncChar, nosync, withPromptInputSync)
import Ribosome.Menu.Scratch (menuScratch, menuScratchSized)
import Ribosome.Menu.Test.DeleteCursorTest (test_deleteCursor)
import Ribosome.Menu.Test.Run (unitTestTimes)
import Ribosome.Menu.Test.Util (mkItems, staticMenuItems)
import Ribosome.Test.Embed (testEmbed_)
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.Skip (requireX)
import Ribosome.Test.SocketTmux (testSocketTmux)

pureEvents :: [PromptEvent]
pureEvents =
  [
    Update (Prompt 0 PromptMode.Normal ""),
    Update (Prompt 3 PromptMode.Normal "ite"),
    Mapping "k",
    Mapping "<c-k>",
    Mapping "k",
    Mapping "<cr>"
  ]

items :: [Text]
items =
  (mappend "its" . show <$> [(5 :: Int)..9])
  <>
  (mappend "item" . show <$> [(1 :: Int)..8])

currentEntry ::
  MenuState s =>
  MenuWidget s r Text
currentEntry = do
  CursorIndex s <- readCursor
  fs <- sortEntries . view entries <$> readState
  maybe menuOk menuSuccess (fs ^? ix (fromIntegral s) . #item . #text)

mappings ::
  MenuState s =>
  Mappings s r Text
mappings =
  [("<cr>", currentEntry)]

test_pureInput :: UnitTest
test_pureInput =
  testEmbed_ $ promptInput pureEvents do
    result <- windowMenu (mkItems items) (modal Fuzzy) opts mappings
    MenuResult.Success "item4" === result
  where
    opts =
      def
      & #prompt .~ startInsert
      & #items .~ menuScratchSized 4

nativeChars :: [SyncChar]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

test_nativeWindow :: UnitTest
test_nativeWindow =
  testEmbed_ $ interpretFilter $ interpretSingleWindowMenu $ runMenuUi (mkItems items) (modal Fuzzy) do
    result <- flip (withMenuUi opts) mappings \ m -> do
      withPromptInputSync nativeChars do
        menuMaps (insertAt @2 <$> m)
    MenuResult.Success "item4" === result
  where
    opts =
      def
      & #prompt .~ startInsert
      & #items .~ menuScratchSized 4

test_interruptWindow :: UnitTest
test_interruptWindow =
  testEmbed_ $ interpretFilter $ interpretSingleWindowMenu $ runMenuUi (mkItems items) (modal Fuzzy) do
    result <- flip (withMenuUi opts) mappings \ m -> do
      withPromptInputSync ["i", "<c-c>", "<cr>"] do
        menuMaps (insertAt @2 <$> m)
    MenuResult.Aborted === result
    assertEq 1 . length =<< vimGetWindows
  where
    opts =
      def & #prompt .~ startInsert

test_windowOnlyInsert :: UnitTest
test_windowOnlyInsert =
  testEmbed_ $ interpretFilter $ interpretSingleWindowMenu $ runMenuUi (mkItems items) (modal Fuzzy) do
      result <- flip (withMenuUi opts) mappings \ m -> do
        withPromptInputSync ["i", nosync "<esc>", "<cr>"] do
          menuMaps (insertAt @2 <$> m)
      MenuResult.Aborted === result
  where
    opts =
      def & #prompt .~ onlyInsert

test_quit :: UnitTest
test_quit =
  testEmbed_ $ interpretFilter do
    result <- testStaticNvimMenu @() @(Modal Filter Text) [] def (modal Fuzzy) (menuScratchSized 4) defaultMappings do
      sendMapping "<esc>"
      MenuTest.result
    MenuResult.Aborted === result
    assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_scrollUp :: UnitTest
test_scrollUp =
  testEmbed_ do
    MenuResult.Success a <- testStaticNvimMenu its def (modal Fuzzy) (menuScratch & #maxSize ?~ 4) maps do
      waitEvent "initial render" Rendered
      traverse_ sendMappingPrompt (replicate 20 "k" <> ["<cr>"])
      MenuTest.result
    4 === length a
  where
    maps =
      defaultMappings <> [("<cr>", content)]
    content = do
      [_, itemsBuffer, _, _] <- vimGetBuffers
      menuSuccess =<< bufferContent itemsBuffer
    its =
      staticMenuItems (replicate 100 "item")

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
  testSocketTmux $ interpretFilter do
    result <- testStaticNvimMenu @() [] conf (TestState (modal (TestMode Fuzzy 0)) "empty") def defaultMappings do
      status <- MenuUi.statusScratch
      assertEq ["message: empty"] . drop 1 =<< bufferContent (status ^. #buffer)
      awaitScreenshot False "menu-bottom-status" 0
      withPromptInputSync [nosync "<esc>"] do
        MenuTest.result
    MenuResult.Aborted === result
  where
    conf = confSet #nativePrompt True def

test_nvimMenu :: TestTree
test_nvimMenu =
  testGroup "nvim menu" [
    unitTest "pure" test_pureInput,
    unitTestTimes 10 "native window" test_nativeWindow,
    unitTestTimes 10 "interrupt window" test_interruptWindow,
    unitTest "close scratch when quitting" test_quit,
    unitTest "scroll up" test_scrollUp,
    requireX (unitTestTimes 3 "extra bottom status message") test_bottomStatus,
    unitTest "correctness of cursor after delete" test_deleteCursor
  ]
