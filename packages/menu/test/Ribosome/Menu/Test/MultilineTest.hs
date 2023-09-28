module Ribosome.Menu.Test.MultilineTest where

import qualified Data.List.NonEmpty as NonEmpty
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (Hedgehog, UnitTest, (===))

import Ribosome.Api.Buffer (bufferContent)
import Ribosome.Api.Window (windowLine)
import qualified Ribosome.Data.ScratchState
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Action (menuFocusItem)
import Ribosome.Menu.Data.Filter (Filter (Substring))
import Ribosome.Menu.Data.MenuItem (simpleMenuItemLines)
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Effect.MenuTest (MenuTest, quit, result, sendMappingPrompt)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)
import Ribosome.Menu.Mappings (defaultMappings)
import Ribosome.Menu.MenuTest (runTestMenu, testStaticNvimMenu)
import Ribosome.Menu.Prompt.Data.PromptConfig (startInsert)
import Ribosome.Menu.Scratch (menuScratchSized)
import Ribosome.Test.Embed (testEmbed_)
import Ribosome.Test.Error (testError)
import Ribosome.Test.Wait ((<--))

cycle ::
  HasCallStack =>
  Members [MenuTest i a, Rpc, Hedgehog IO, ChronosTime, Race, Error Failure, Embed IO] r =>
  Text ->
  ScratchState ->
  [Text] ->
  Int ->
  Sem r ()
cycle char items target line = do
  withFrozenCallStack do
    sendMappingPrompt char
    target <-- bufferContent items.buffer
    line <-- windowLine items.window

up ::
  HasCallStack =>
  Members [MenuTest i a, Rpc, Hedgehog IO, ChronosTime, Race, Error Failure, Embed IO] r =>
  ScratchState ->
  [Text] ->
  Int ->
  Sem r ()
up = withFrozenCallStack (cycle "k")

down ::
  HasCallStack =>
  Members [MenuTest i a, Rpc, Hedgehog IO, ChronosTime, Race, Error Failure, Embed IO] r =>
  ScratchState ->
  [Text] ->
  Int ->
  Sem r ()
down = withFrozenCallStack (cycle "j")

itemsMultiline :: [NonEmpty Text]
itemsMultiline =
  [["0"], ["1", "2", "3", "4", "5"], ["6", "7"], ["8", "9"]]

bufferTarget1 :: [Text]
bufferTarget1 =
  [
    "7",
    "1",
    "2",
    "3",
    "4",
    "5",
    "0"
  ]

bufferTarget2 :: [Text]
bufferTarget2 =
  [
    "6",
    "7",
    "1",
    "2",
    "3",
    "4",
    "5"
  ]

bufferTarget3 :: [Text]
bufferTarget3 =
  [
    "8",
    "9",
    "6",
    "7",
    "1",
    "2",
    "3"
  ]

test_multiline :: UnitTest
test_multiline =
  testEmbed_ do
    runTestMenu startInsert $ interpretFilter do
      r <- testError $ testStaticNvimMenu its def (modal Substring) (menuScratchSized 7 & #maxSize ?~ 7) maps do
        items <- MenuUi.itemsScratch
        bufferTarget1 <-- bufferContent items.buffer
        6 <-- windowLine items.window
        sendMappingPrompt "k"
        1 <-- windowLine items.window
        up items bufferTarget2 0
        up items bufferTarget3 0
        down items bufferTarget3 2
        down items bufferTarget2 2
        down items bufferTarget1 6
        down items bufferTarget3 0
        sendMappingPrompt "<cr>"
        quit
        result
      Success (Just (simpleMenuItemLines () ["8", "9"])) === r
  where
    maps = defaultMappings <> [("<cr>", menuFocusItem)]
    its = simpleMenuItemLines () <$> itemsMultiline

itemsCramped :: NonEmpty (NonEmpty Text)
itemsCramped =
  [["1", "2", "3", "4", "5"], ["6", "7", "8", "9"], ["10", "11", "12", "13"]]

crampedTarget1 :: [Text]
crampedTarget1 =
  ["1", "2"]

crampedTarget2 :: [Text]
crampedTarget2 =
  ["6", "7"]

crampedTarget3 :: [Text]
crampedTarget3 =
  ["10", "11"]

test_multilineCramped :: UnitTest
test_multilineCramped =
  testEmbed_ do
    runTestMenu startInsert $ interpretFilter do
      r <- testError $ testStaticNvimMenu its def (modal Substring) (menuScratchSized 2 & #maxSize ?~ 2) maps do
        items <- MenuUi.itemsScratch
        crampedTarget1 <-- bufferContent items.buffer
        0 <-- windowLine items.window
        up items crampedTarget2 0
        up items crampedTarget3 0
        up items crampedTarget1 0
        down items crampedTarget3 0
        sendMappingPrompt "<cr>"
        quit
        result
      Success (Just (simpleMenuItemLines () (NonEmpty.last itemsCramped))) === r
  where
    maps = defaultMappings <> [("<cr>", menuFocusItem)]
    its = simpleMenuItemLines () <$> toList itemsCramped
