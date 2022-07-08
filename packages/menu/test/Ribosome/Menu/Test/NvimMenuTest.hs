module Ribosome.Menu.Test.NvimMenuTest where

import qualified Data.Map.Strict as Map
import Polysemy.Test (UnitTest, assertEq, runTestAuto, unitTest, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import Test.Tasty (TestTree, testGroup)
import Time (MilliSeconds (MilliSeconds))

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Data.ScratchOptions (ScratchOptions (maxSize))
import Ribosome.Effect.Scratch (Scratch)
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
import Ribosome.Menu.Interpreter.MenuConsumer (Mappings, basic, withMappings)
import Ribosome.Menu.Interpreter.MenuState (interpretMenu)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Interpreter.PromptInput (interpretPromptInputCharList, interpretPromptInputNvim)
import Ribosome.Menu.Nvim (nvimMenu, nvimMenuWith, runNvimMenu, staticNvimMenu)
import Ribosome.Menu.NvimRenderer (computeView, entrySlice)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag (StartInsert))
import Ribosome.Menu.Prompt.Run (withPromptInput)
import Ribosome.Menu.Test.Menu (staticNvimMenuTestDef)
import Ribosome.Test.Embed (testEmbed_)
import Ribosome.Test.Error (resumeTestError)

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

test_nvimMenuPure :: UnitTest
test_nvimMenuPure =
  testEmbed_ $ interpretMenu $ interpretPromptControl $ interpretPromptInputCharList pureChars do
    result <- resumeTestError @Scratch $ withMappings mappings do
      runNvimMenu $ nvimMenuWith def { maxSize = Just 4 } (menuItems items) [StartInsert]
    MenuResult.Success "item4" === result

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

test_nvimMenuNative :: UnitTest
test_nvimMenuNative =
  testEmbed_ $ interpretMenu $ interpretPromptControl $ interpretPromptInputNvim do
    withPromptInput (Just (MilliSeconds 10)) nativeChars do
      result <- resumeTestError @Scratch $ withMappings mappings do
        runNvimMenu $ nvimMenuWith def { maxSize = Just 4 } (menuItems items) [StartInsert]
      MenuResult.Success "item4" === result

test_nvimMenuInterrupt :: UnitTest
test_nvimMenuInterrupt =
  testEmbed_ $ interpretMenu $ interpretPromptControl $ interpretPromptInputNvim do
    assertEq MenuResult.Aborted =<< withPromptInput (Just (MilliSeconds 50)) ["<c-c>", "<cr>"] do
      resumeTestError @Scratch $ basic @() do
        runNvimMenu $ nvimMenuWith def (menuItems items) [StartInsert]
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
  testEmbed_ $ interpretMenu $ withMappings navMappings do
    result <- staticNvimMenuTestDef conf do
      traverse_ sendCharWait navChars
      MenuTest.result
    MenuResult.Success "toem" === result
  where
    conf =
      staticNvimMenu (staticMenuItems items)
      & #scratch . #maxSize .~ Just 4

test_nvimMenuQuit :: UnitTest
test_nvimMenuQuit =
  testEmbed_ $ interpretMenu $ basic do
    result <- staticNvimMenuTestDef @Void @Void (staticNvimMenu []) do
      sendChar "esc"
      MenuTest.result
    MenuResult.NoAction === result
    assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_menuScrollUp :: UnitTest
test_menuScrollUp =
  testEmbed_ $ interpretMenu $ withMappings (Map.singleton "cr" content) do
      MenuResult.Success a <- staticNvimMenuTestDef (nvimMenu (menuItems its) & #scratch . #maxSize ?~ 4) do
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
    unitTest "pure" test_nvimMenuPure,
    unitTest "native" test_nvimMenuNative,
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
