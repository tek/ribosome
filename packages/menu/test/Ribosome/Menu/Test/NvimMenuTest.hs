module Ribosome.Menu.Test.NvimMenuTest where

import Polysemy.Test (UnitTest, assertEq, runTestAuto, unitTest, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import Test.Tasty (TestTree, testGroup)
import Time (MilliSeconds (MilliSeconds))

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Host.Api.Effect (bufferGetName, vimGetBuffers, vimGetWindows)
import Ribosome.Menu.Action (menuOk, menuSuccess)
import Ribosome.Menu.Combinators (sortedEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (intEntries)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuState (MenuWidget)
import Ribosome.Menu.Data.MenuView (MenuView (MenuView))
import Ribosome.Menu.Effect.MenuState (MenuState, readCursor, readPrompt, viewMenu)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendChar, sendCharWait)
import Ribosome.Menu.Interpreter.Menu (interpretNvimMenuFinal, runNvimMenu, runNvimMenuFinal)
import Ribosome.Menu.Interpreter.MenuConsumer (Mappings, basic, withMappings)
import Ribosome.Menu.Interpreter.NvimPromptInput (interpretNvimPromptInputCharList)
import Ribosome.Menu.Main (menu)
import Ribosome.Menu.MenuTest (runStaticTestMenu, testStaticNvimMenu)
import Ribosome.Menu.Nvim (menuScratch, menuScratchSized)
import Ribosome.Menu.NvimRenderer (computeView, entrySlice)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag (StartInsert))
import Ribosome.Menu.Prompt.Run (withPromptInput)
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

pureChars :: [Text]
pureChars =
  ["i", "t", "e", "esc", "k", "k", "k", "cr"]

items :: [Text]
items =
  (mappend "item" . show <$> [(1 :: Int)..8])
  <>
  (mappend "its" . show <$> [(5 :: Int)..9])

exec ::
  Member (MenuState i) r =>
  MenuWidget r Text
exec = do
  CursorIndex s <- readCursor
  fs <- viewMenu sortedEntries
  maybe menuOk menuSuccess (fs ^? ix s . #item . #text)

mappings ::
  Member (MenuState i) r =>
  Mappings r Text
mappings =
  [("cr", exec)]

test_nvimMenuPureInput :: UnitTest
test_nvimMenuPureInput =
  testEmbed_ $ interpretNvimMenuFinal $ interpretNvimPromptInputCharList True pureChars do
    result <- runNvimMenu (menuItems items) [StartInsert] (menuScratchSized 4) $ withMappings mappings do
      menu
    MenuResult.Success "item4" === result

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

test_nvimMenuNativeInput :: UnitTest
test_nvimMenuNativeInput =
  testEmbed_ do
    result <- runNvimMenuFinal (menuItems items) [StartInsert] (menuScratchSized 4) $ withMappings mappings do
      withPromptInput (Just (MilliSeconds 10)) nativeChars do
        menu
    MenuResult.Success "item4" === result

test_nvimMenuInterrupt :: UnitTest
test_nvimMenuInterrupt =
  testEmbed_ do
    result <- runNvimMenuFinal (menuItems items) [StartInsert] def $ basic @() do
      withPromptInput (Just (MilliSeconds 10)) ["i", "<c-c>", "<cr>"] do
        menu
    MenuResult.Aborted === result
    assertEq 1 . length =<< vimGetWindows

returnPrompt ::
  Member (MenuState i) r =>
  Sem r (Maybe (MenuAction Text))
returnPrompt = do
  Prompt _ _ (PromptText text) <- readPrompt
  menuSuccess text

navChars :: [Text]
navChars =
  ["i", "i", "t", "e", "m", "1", "bs", "esc", "h", "h", "h", "h", "h", "x", "a", "o", "cr"]

navMappings ::
  Member (MenuState Text) r =>
  Mappings r Text
navMappings =
  [("cr", returnPrompt)]

test_nvimMenuNav :: UnitTest
test_nvimMenuNav =
  testEmbed_ $ runStaticTestMenu (staticMenuItems items) [] navMappings do
    result <- testStaticNvimMenu (menuScratchSized 4) do
      traverse_ sendCharWait navChars
      MenuTest.result
    MenuResult.Success "toem" === result

test_nvimMenuQuit :: UnitTest
test_nvimMenuQuit =
  testEmbed_ $ runStaticTestMenu [] [] mempty do
    result <- testStaticNvimMenu @() @Text (menuScratchSized 4) do
      sendChar "esc"
      MenuTest.result
    MenuResult.NoAction === result
    assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_menuScrollUp :: UnitTest
test_menuScrollUp =
  testEmbed_ $ runStaticTestMenu (staticMenuItems its) [] [("cr", content)] do
      MenuResult.Success a <- testStaticNvimMenu (menuScratch & #maxSize ?~ 4) do
        traverse_ sendCharWait (replicate 20 "k" <> ["cr"])
        MenuTest.result
      4 === length a
  where
    content = do
      [_, mb] <- vimGetBuffers
      menuSuccess =<< bufferContent mb
    its =
      replicate 100 "item"

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
    unitTest "pure" test_nvimMenuPureInput,
    unitTest "native" test_nvimMenuNativeInput,
    unitTest "interrupt" test_nvimMenuInterrupt,
    unitTest "navigation" test_nvimMenuNav,
    unitTest "close scratch when quitting" test_nvimMenuQuit,
    unitTest "new view after scrolling up" test_viewScrollUp,
    unitTest "new view after scrolling down" test_viewScrollDown,
    unitTest "new view after moving cursor" test_viewMoveCursor,
    unitTest "new view after initial render" test_viewInitial,
    unitTest "extract a slice of entries from the score map" test_entrySlice,
    unitTest "scroll up" test_menuScrollUp
  ]
