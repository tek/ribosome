module Ribosome.Menu.Test.NvimMenuTest where

import Lens.Micro.Mtl (view)
import Polysemy.Test (UnitTest, assertEq, runTestAuto, unitTest, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Host.Api.Effect (bufferGetName, vimGetBuffers, vimGetWindows)
import Ribosome.Menu.Action (MenuWidget, menuOk, menuSuccess)
import Ribosome.Menu.Combinators (sortEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (intEntries)
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuView (MenuView (MenuView))
import Ribosome.Menu.Effect.MenuState (readCursor, readItems)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMapping, sendMappingWait, waitEvent)
import Ribosome.Menu.Effect.MenuUi (WindowMenu)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuLoop (interpretNvimMenu, promptInput)
import Ribosome.Menu.Loop (nvimMenu, nvimMenuLoop, runMenu)
import Ribosome.Menu.Mappings (Mappings, defaultMappings)
import Ribosome.Menu.MenuTest (testStaticNvimMenu)
import Ribosome.Menu.NvimRenderer (computeView, entrySlice)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (onlyInsert, startInsert)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent (Mapping, Update))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Run (SyncChar, nosync, withPromptInputSync)
import Ribosome.Menu.Scratch (menuScratch, menuScratchSized)
import Ribosome.Menu.Test.Run (unitTestTimes)
import Ribosome.Test.Embed (testEmbed_)

staticMenuItems ::
  [Text] ->
  [MenuItem Text]
staticMenuItems =
  fmap (simpleMenuItem "name")

menuItems ::
  [Text] ->
  SerialT IO (MenuItem Text)
menuItems =
  Stream.fromList . fmap (simpleMenuItem "name")

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

currentEntries :: MenuWidget Filter i r Text
currentEntries = do
  CursorIndex s <- readCursor
  fs <- sortEntries . view #entries <$> readItems
  maybe menuOk menuSuccess (fs ^? ix s . #item . #text)

mappings :: Mappings Filter i r Text
mappings =
  [("<cr>", currentEntries)]

test_pureInput :: UnitTest
test_pureInput =
  testEmbed_ $ promptInput pureEvents do
    result <- nvimMenu (menuItems items) Fuzzy opts mappings
    MenuResult.Success "item4" === result
  where
    opts =
      def
      & #prompt .~ startInsert
      & #items .~ menuScratchSized 4

nativeChars :: [SyncChar]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

nativeWindowChars :: [SyncChar]
nativeWindowChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

test_nativeWindow :: UnitTest
test_nativeWindow =
  testEmbed_ $ defaultFilter $ interpretNvimMenu $ runMenu (menuItems items) Fuzzy do
    result <- withPromptInputSync nativeWindowChars do
      nvimMenuLoop @WindowMenu opts mappings
    MenuResult.Success "item4" === result
  where
    opts =
      def
      & #prompt .~ startInsert
      & #items .~ menuScratchSized 4

test_interruptWindow :: UnitTest
test_interruptWindow =
  testEmbed_ $ defaultFilter do
    result <- interpretNvimMenu $ runMenu (menuItems items) Fuzzy do
      withPromptInputSync ["i", "<c-c>", "<cr>"] do
        nvimMenuLoop @WindowMenu opts mappings
    MenuResult.Aborted === result
    assertEq 1 . length =<< vimGetWindows
  where
    opts =
      def & #prompt .~ startInsert

test_windowOnlyInsert :: UnitTest
test_windowOnlyInsert =
  testEmbed_ $ defaultFilter do
    result <- interpretNvimMenu $ runMenu (menuItems items) Fuzzy do
      withPromptInputSync ["i", nosync "<esc>", "<cr>"] do
        nvimMenuLoop @WindowMenu opts mappings
    MenuResult.Aborted === result
  where
    opts =
      def & #prompt .~ onlyInsert

test_quit :: UnitTest
test_quit =
  testEmbed_ $ defaultFilter do
    result <- testStaticNvimMenu @() @Text [] def Fuzzy (menuScratchSized 4) defaultMappings do
      sendMapping "<esc>"
      MenuTest.result
    MenuResult.Aborted === result
    assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_scrollUp :: UnitTest
test_scrollUp =
  testEmbed_ $ defaultFilter do
    MenuResult.Success a <- testStaticNvimMenu its def Fuzzy (menuScratch & #maxSize ?~ 4) maps do
      waitEvent "initial render" Rendered
      traverse_ sendMappingWait (replicate 20 "k" <> ["<cr>"])
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

nmenuBot :: MenuView
nmenuBot =
  MenuView 4 0 4 4

nmenuTop :: MenuView
nmenuTop =
  MenuView 9 5 5 0

nmenuMid :: MenuView
nmenuMid =
  MenuView 7 3 5 3

test_viewScrollUp :: UnitTest
test_viewScrollUp =
  runTestAuto do
    MenuView 6 2 6 4 === computeView 6 5 10 nmenuBot

test_viewScrollDown :: UnitTest
test_viewScrollDown =
  runTestAuto do
    MenuView 8 4 4 0 === computeView 4 5 10 nmenuTop

test_viewMoveCursor :: UnitTest
test_viewMoveCursor =
  runTestAuto do
    MenuView 7 3 4 2 === computeView 4 5 10 nmenuMid

test_viewInitial :: UnitTest
test_viewInitial =
  runTestAuto do
    MenuView 0 0 0 0 === computeView 0 1 10 (MenuView 0 0 0 0)

test_entrySlice :: UnitTest
test_entrySlice =
  runTestAuto do
    6 === length (entrySlice ents 25 30)
  where
    ents =
      intEntries (zip (repeat 0) [1..100])

test_nvimMenu :: TestTree
test_nvimMenu =
  testGroup "nvim menu" [
    unitTest "pure" test_pureInput,
    unitTestTimes 10 "native window" test_nativeWindow,
    unitTestTimes 10 "interrupt window" test_interruptWindow,
    unitTest "close scratch when quitting" test_quit,
    unitTest "scroll up" test_scrollUp,
    unitTest "new view after scrolling up" test_viewScrollUp,
    unitTest "new view after scrolling down" test_viewScrollDown,
    unitTest "new view after moving cursor" test_viewMoveCursor,
    unitTest "new view after initial render" test_viewInitial,
    unitTest "extract a slice of entries from the score map" test_entrySlice
  ]
