module Ribosome.Menu.Test.NvimMenuTest where

import Control.Lens (element, use, (^?))
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
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.MenuState (
  MenuRead,
  MenuSem,
  MenuWidget',
  MenuWrite,
  SemS (SemS),
  menuRead,
  menuWrite,
  semState,
  )
import Ribosome.Menu.Data.MenuView (MenuView (MenuView))
import Ribosome.Menu.Interpreter.MenuConsumer (Mappings, basic, withMappings)
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNvim)
import Ribosome.Menu.ItemLens (cursor)
import Ribosome.Menu.Main (interpretMenu)
import Ribosome.Menu.Nvim (nvimMenuDef, staticNvimMenuDef)
import Ribosome.Menu.NvimRenderer (computeView, entrySlice)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag (StartInsert), PromptInput)
import Ribosome.Menu.Prompt.Input (promptInput, promptInputWith)
import Ribosome.Menu.Prompt.Nvim (getCharStream)
import Ribosome.Menu.Prompt.Run (withPromptInput)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Test.Embed (testEmbed_)
import Ribosome.Test.Error (resumeTestError)

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
  MenuWrite i r =>
  MenuWidget' r Text
exec =
  menuWrite do
    semState do
      CursorIndex s <- use cursor
      fs <- use sortedEntries
      SemS (maybe menuOk menuSuccess (fs ^? element s . #item . #text))

promptConfig ::
  PromptInput ->
  PromptConfig r
promptConfig source =
  PromptConfig source basicTransition [StartInsert]

mappings ::
  MenuWrite Text r =>
  Mappings r Text
mappings =
  Map.fromList [("cr", exec)]

test_nvimMenuPure :: UnitTest
test_nvimMenuPure =
  testEmbed_ $ interpretMenu do
    result <- resumeTestError @Scratch $ interpretPromptRendererNvim $ withMappings mappings do
      nvimMenuDef def { maxSize = Just 4 } (menuItems items) (promptConfig (promptInput pureChars))
    MenuResult.Success "item4" === result

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

test_nvimMenuNative :: UnitTest
test_nvimMenuNative =
  testEmbed_ $ interpretMenu do
    inp <- getCharStream (MilliSeconds 10)
    withPromptInput (Just (MilliSeconds 10)) nativeChars do
      result <- resumeTestError @Scratch $ interpretPromptRendererNvim $ withMappings mappings do
        nvimMenuDef def { maxSize = Just 4 } (menuItems items) (promptConfig inp)
      MenuResult.Success "item4" === result

test_nvimMenuInterrupt :: UnitTest
test_nvimMenuInterrupt =
  testEmbed_ $ interpretMenu do
    conf <- promptConfig <$> getCharStream (MilliSeconds 10)
    assertEq MenuResult.Aborted =<< withPromptInput (Just (MilliSeconds 50)) ["<c-c>", "<cr>"] do
      resumeTestError @Scratch $ interpretPromptRendererNvim $ basic @() do
        nvimMenuDef def (menuItems items) conf
    assertEq 1 . length =<< vimGetWindows

returnPrompt ::
  MenuSem Text r (Maybe (MenuAction Text))
returnPrompt = do
  Prompt _ _ (PromptText text) <- semState (use #prompt)
  menuSuccess text

navChars :: [Text]
navChars =
  ["i", "t", "e", "m", "1", "<bs>", "<esc>", "h", "h", "h", "h", "h", "x", "a", "o", "<cr>"]

navMappings ::
  MenuRead Text r =>
  Mappings r Text
navMappings =
  Map.fromList [("cr", menuRead returnPrompt)]

test_nvimMenuNav :: UnitTest
test_nvimMenuNav =
  testEmbed_ $ interpretMenu do
    assertEq (MenuResult.Success "toem") =<< do
      withPromptInput (Just (MilliSeconds 10)) navChars do
        source <- getCharStream (MilliSeconds 10)
        resumeTestError @Scratch $ interpretPromptRendererNvim $ withMappings navMappings do
          nvimMenuDef def { maxSize = Just 4 } (menuItems items) (promptConfig source)

test_nvimMenuQuit :: UnitTest
test_nvimMenuQuit =
  testEmbed_ $ interpretMenu do
    resumeTestError @Scratch $ interpretPromptRendererNvim $ basic do
      void $ staticNvimMenuDef @() def [] (PromptConfig inp basicTransition [])
      assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers
  where
    inp =
      promptInputWith (Just 0.5) Nothing (Stream.repeat "esc")

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

test_menuScrollUp :: UnitTest
test_menuScrollUp =
  testEmbed_ $ interpretMenu do
    resumeTestError @Scratch $ interpretPromptRendererNvim $ basic do
      let prompt = PromptConfig (promptInputWith (Just 0.2) (Just 0.01) chars) basicTransition []
      Success a <-  withMappings (Map.singleton "cr" content) do
        nvimMenuDef def { maxSize = Just 4 } (menuItems its) prompt
      4 === length a
  where
    chars =
      Stream.fromList (replicate 20 "k" <> ["cr"])
    content = do
      [_, mb] <- vimGetBuffers
      menuSuccess =<< bufferContent mb
    its =
      replicate 100 "item"

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
