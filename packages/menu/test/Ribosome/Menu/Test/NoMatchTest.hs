module Ribosome.Menu.Test.NoMatchTest where

import Polysemy.Test (UnitTest)

import Ribosome.Menu.Data.Filter (substring)
import Ribosome.Menu.Data.State (modal)
import Ribosome.Menu.Effect.MenuTest (quit, setPromptWait)
import Ribosome.Menu.Scratch (menuScratchSized)
import Ribosome.Menu.Test.Menu (awaitItemsBuffer)
import Ribosome.Menu.Test.Run (testStaticNvimMenuSimple)
import Ribosome.Test.Embed (testEmbed_)
import Ribosome.Test.Error (testError)

itemsNoMatch :: [NonEmpty Text]
itemsNoMatch =
  [["item 1"], ["item 2"], ["item 3"], ["item 4"]]

initialBuffer :: [Text]
initialBuffer =
  [
    "item 4",
    "item 3",
    "item 2",
    "item 1"
  ]

test_filterNoMatch :: UnitTest
test_filterNoMatch =
  testEmbed_ do
    testError $ testStaticNvimMenuSimple itemsNoMatch def (modal substring) (menuScratchSized 4) mempty do
      awaitItemsBuffer initialBuffer
      setPromptWait "3"
      awaitItemsBuffer ["item 3"]
      setPromptWait "3x"
      awaitItemsBuffer [""]
      quit
