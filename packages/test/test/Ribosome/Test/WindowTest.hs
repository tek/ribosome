module Ribosome.Test.WindowTest where

import Hedgehog ((===))
import TestError (RiboTest)

import Ribosome.Api.Window (ensureMainWindow)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (bufferSetOption, nvimCommand, vimGetCurrentBuffer, vimGetCurrentWindow, vimGetWindows)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (unitTestDef')

setCurrentNofile :: RiboTest ()
setCurrentNofile = do
  buf <- vimGetCurrentBuffer
  bufferSetOption buf "buftype" (toMsgpack ("nofile" :: Text))

createNofile :: RiboTest Window
createNofile = do
  initialWindow <- vimGetCurrentWindow
  nvimCommand "new"
  setCurrentNofile
  return initialWindow

findMainWindowExistingTest :: RiboTest ()
findMainWindowExistingTest = do
  initialWindow <- createNofile
  (initialWindow ===) =<< ensureMainWindow

test_findMainWindowExisting :: UnitTest
test_findMainWindowExisting =
  unitTestDef' findMainWindowExistingTest

findMainWindowCreateTest :: RiboTest ()
findMainWindowCreateTest = do
  setCurrentNofile
  void createNofile
  void ensureMainWindow
  (3 ===) =<< length <$> vimGetWindows

test_findMainWindowCreate :: UnitTest
test_findMainWindowCreate =
  unitTestDef' findMainWindowCreateTest
