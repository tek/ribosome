module Ribosome.Test.UndoTest where

import Polysemy.Test (UnitTest, assertEq)

import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Register (setregLine, unnamedRegister)
import Ribosome.Api.Window (setCurrentCursor)
import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Test.Run (embedTest_)

test_undo :: UnitTest
test_undo =
  embedTest_ do
    setCurrentBufferContent orig
    setCurrentCursor 1 0
    setregLine unnamedRegister new
    nvimCommand "normal! p"
    assertEq (orig <> new) =<< currentBufferContent
    nvimCommand "undo"
    assertEq orig =<< currentBufferContent
  where
    orig =
      ["line1", "line2"]
    new =
      ["new1", "new2"]
