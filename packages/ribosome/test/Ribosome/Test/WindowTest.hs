module Ribosome.Test.WindowTest where

import Polysemy.Test (UnitTest, assertEq, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Window (ensureMainWindow)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (bufferSetOption, nvimCommand, vimGetCurrentBuffer, vimGetCurrentWindow, vimGetWindows)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Test.Run (embedTest_)

setCurrentNofile ::
  Member Rpc r =>
  Sem r ()
setCurrentNofile = do
  buf <- vimGetCurrentBuffer
  bufferSetOption buf "buftype" (toMsgpack ("nofile" :: Text))

createNofile ::
  Member Rpc r =>
  Sem r Window
createNofile = do
  initialWindow <- vimGetCurrentWindow
  nvimCommand "new"
  setCurrentNofile
  pure initialWindow

test_findMainWindowExisting :: UnitTest
test_findMainWindowExisting =
  embedTest_ do
    initialWindow <- createNofile
    (initialWindow ===) =<< ensureMainWindow

test_findMainWindowCreate :: UnitTest
test_findMainWindowCreate =
  embedTest_ do
    setCurrentNofile
    void createNofile
    void ensureMainWindow
    assertEq 3 . length =<< vimGetWindows

test_window :: TestTree
test_window =
  testGroup "window" [
    unitTest "find existing" test_findMainWindowExisting,
    unitTest "find create" test_findMainWindowCreate
  ]
