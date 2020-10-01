{-# OPTIONS_GHC -F -pgmF htfpp #-}

module WindowSpec (htf_thisModulesTests) where

import Ribosome.Api.Window (ensureMainWindow)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (bufferSetOption, vimCommand, vimGetCurrentBuffer, vimGetCurrentWindow, vimGetWindows)
import Ribosome.Test.Unit (unitSpecDef')
import Test.Framework
import TestError (RiboT)

setCurrentNofile :: RiboT ()
setCurrentNofile = do
  buf <- vimGetCurrentBuffer
  bufferSetOption buf "buftype" (toMsgpack ("nofile" :: Text))

createNofile :: RiboT Window
createNofile = do
  initialWindow <- vimGetCurrentWindow
  vimCommand "new"
  setCurrentNofile
  return initialWindow

findMainWindowExistingSpec :: RiboT ()
findMainWindowExistingSpec = do
  initialWindow <- createNofile
  gassertEqual initialWindow =<< ensureMainWindow

test_findMainWindowExisting :: IO ()
test_findMainWindowExisting =
  unitSpecDef' findMainWindowExistingSpec

findMainWindowCreateSpec :: RiboT ()
findMainWindowCreateSpec = do
  setCurrentNofile
  void createNofile
  void ensureMainWindow
  gassertEqual 3 =<< length <$> vimGetWindows

test_findMainWindowCreate :: IO ()
test_findMainWindowCreate =
  unitSpecDef' findMainWindowCreateSpec
