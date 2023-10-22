module Ribosome.Menu.Test.NvimMenuTest where

import Lens.Micro.Mtl (view)
import Polysemy.Test (UnitTest, assertEq, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Host.Api.Data (bufferGetName, vimGetBuffers)
import Ribosome.Menu.Action (MenuWidget, menuOk, menuSuccess)
import Ribosome.Menu.App (MenuApp, defaultHandlers)
import Ribosome.Menu.Class.MenuState (MenuState, entries)
import Ribosome.Menu.Combinators (sortEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Filter (fuzzy)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Rendered))
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Effect.Menu (readCursor, readState)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (sendMapping, sendMappingRender, waitEvent)
import Ribosome.Menu.Interpreter.Menu (promptInput)
import Ribosome.Menu.Loop (windowMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), startInsert)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent (Mapping, Update))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Scratch (menuScratch, menuScratchSized)
import Ribosome.Menu.Test.DeleteCursorTest (test_deleteCursor)
import Ribosome.Menu.Test.Run (testStaticNvimMenu)
import Ribosome.Menu.Test.Util (mkItems, staticMenuItems)
import Ribosome.Test.Embed (testEmbed_)

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

app ::
  MenuState s =>
  MenuApp s r Text
app =
  [("<cr>", currentEntry)]

test_pureInput :: UnitTest
test_pureInput =
  testEmbed_ $ promptInput pureEvents do
    result <- windowMenu (mkItems items) (modal fuzzy) opts app
    MenuResult.Success "item4" === result
  where
    opts =
      def
      & #prompt . #modes .~ startInsert
      & #items .~ menuScratchSized 4

test_quit :: UnitTest
test_quit =
  testEmbed_ do
    result <- testStaticNvimMenu @() [] def (modal @Text fuzzy) (menuScratchSized 4) defaultHandlers do
      sendMapping "<esc>"
      MenuTest.result
    MenuResult.Aborted === result
    assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_scrollUp :: UnitTest
test_scrollUp =
  testEmbed_ do
    MenuResult.Success a <- testStaticNvimMenu its def (modal fuzzy) (menuScratch & #maxSize ?~ 4) maps do
      waitEvent "initial render" Rendered
      traverse_ sendMappingRender (replicate 20 "k")
      sendMapping "<cr>"
      MenuTest.result
    4 === length a
  where
    maps =
      defaultHandlers <> [("<cr>", content)]
    content = do
      [_, itemsBuffer, _, _] <- vimGetBuffers
      menuSuccess =<< bufferContent itemsBuffer
    its =
      staticMenuItems (replicate 100 "item")

test_nvimMenu :: TestTree
test_nvimMenu =
  testGroup "nvim menu" [
    unitTest "pure" test_pureInput,
    unitTest "close scratch when quitting" test_quit,
    unitTest "scroll up" test_scrollUp,
    unitTest "correctness of cursor after delete" test_deleteCursor
  ]
