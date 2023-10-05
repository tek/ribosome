module Ribosome.Menu.Test.NativeInputTest where

import Conc (timeoutStop)
import Lens.Micro.Mtl (view)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (UnitTest, assertEq, unitTest, unitTestTimes, (===))
import Test.Tasty (TestTree, testGroup)
import Time (Seconds (Seconds))

import Ribosome.Host.Api.Data (vimGetWindows)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Action (MenuWidget, menuOk, menuSuccess, menuUpdatePrompt)
import Ribosome.Menu.App (MenuApp, insert, withInsert)
import Ribosome.Menu.Class.MenuState (MenuState, entries)
import Ribosome.Menu.Combinators (sortEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Data.WindowConfig (WindowOptions)
import Ribosome.Menu.Effect.Menu (readCursor, readState)
import Ribosome.Menu.Interpreter.Menu (MenuLoopIO, NvimMenuIO, interpretSingleWindowMenu)
import Ribosome.Menu.Loop (menuLoop, windowMenuApp)
import Ribosome.Menu.Prompt.Data.Prompt (onlyInsert, startInsert)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))
import Ribosome.Menu.Prompt.Run (SyncChar, nosync, updating, withPromptInputSync)
import Ribosome.Menu.Scratch (menuScratchSized)
import Ribosome.Menu.Test.Util (mkItems)
import Ribosome.Test.Embed (testEmbed_)

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
  [(withInsert "<cr>", currentEntry), (insert "<c-e>", menuUpdatePrompt ("item" & #mode .~ Insert))]

nativeTest ::
  Members MenuLoopIO r =>
  Members NvimMenuIO r =>
  Members [Stop RpcError, Rpc, ChronosTime] r =>
  WindowOptions ->
  [SyncChar] ->
  Sem r (MenuResult Text)
nativeTest opts chars =
  interpretSingleWindowMenu $ windowMenuApp (mkItems items) (modal Fuzzy) opts app \ papp ->
    withPromptInputSync chars do
      timeoutStop "timed out" (Seconds 3) (menuLoop papp)

test_nativeBasic :: UnitTest
test_nativeBasic =
  testEmbed_ do
    result <- nativeTest opts ["i", "t", "e", updating "<esc>", "k", "<c-k>", "k", "<cr>"]
    MenuResult.Success "item4" === result
  where
    opts =
      def
      & #prompt . #modes .~ startInsert
      & #items .~ menuScratchSized 4

test_nativeInterrupt :: UnitTest
test_nativeInterrupt =
  testEmbed_ do
    result <- nativeTest opts ["i", "<c-c>", "<cr>"]
    MenuResult.Aborted === result
    assertEq 1 . length =<< vimGetWindows
  where
    opts =
      def & #prompt . #modes .~ startInsert

test_nativeOnlyInsert :: UnitTest
test_nativeOnlyInsert =
  testEmbed_ do
    result <- nativeTest opts ["i", nosync "<esc>", "<cr>"]
    MenuResult.Aborted === result
  where
    opts =
      def & #prompt . #modes .~ onlyInsert

test_nativeSetPrompt :: UnitTest
test_nativeSetPrompt =
  testEmbed_ do
    result <- nativeTest opts [updating "<c-e>", "3", "<cr>"]
    MenuResult.Success "item3" === result
  where
    opts =
      def & #prompt . #modes .~ startInsert

test_nativeInput :: TestTree
test_nativeInput =
  testGroup "native input" [
    unitTestTimes 10 "basic" test_nativeBasic,
    unitTestTimes 10 "interrupt window" test_nativeInterrupt,
    unitTestTimes 10 "quit on esc in onlyInsert mode" test_nativeOnlyInsert,
    unitTest "preserve cursor after setting the prompt" test_nativeSetPrompt
  ]
