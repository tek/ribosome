module Ribosome.Scratch where

import Ribosome.Api.Buffer (setBufferContent)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Scratch (Scratch(Scratch))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.Data (
  Buffer,
  Tabpage,
  Window,
  )
import Ribosome.Nvim.Api.IO (
  bufferSetName,
  bufferSetOption,
  vimCommand,
  vimGetCurrentTabpage,
  vimGetCurrentWindow,
  windowGetBuffer,
  windowSetOption,
  )

createScratchTab :: NvimE e m => m Tabpage
createScratchTab = do
  () <- vimCommand "tabnew"
  vimGetCurrentTabpage

createScratchWindow :: NvimE e m => Bool -> Bool -> Maybe Int -> m Window
createScratchWindow vertical wrap size = do
  () <- vimCommand $ prefix ++ cmd
  win <- vimGetCurrentWindow
  () <- windowSetOption win "wrap" (toMsgpack wrap)
  return win
  where
    cmd = if vertical then "vnew" else "new"
    prefix = maybe "" show size

createScratchUiInTab :: NvimE e m => m (Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  return (win, Just tab)

createScratchUiInWindow :: NvimE e m => Bool -> Bool -> Maybe Int -> m (Window, Maybe Tabpage)
createScratchUiInWindow vertical wrap size = do
  win <- createScratchWindow vertical wrap size
  return (win, Nothing)

createScratchUi :: NvimE e m => Bool -> Bool -> Bool -> Maybe Int -> m (Window, Maybe Tabpage)
createScratchUi True _ _ _ =
  createScratchUiInTab
createScratchUi False vertical wrap size =
  createScratchUiInWindow vertical wrap size

configureScratchBuffer :: NvimE e m => Buffer -> String -> m ()
configureScratchBuffer buffer name = do
  () <- bufferSetOption buffer "buftype" (toMsgpack ("nofile" :: String))
  () <- bufferSetOption buffer "bufhidden" (toMsgpack ("wipe" :: String))
  bufferSetName buffer name

setupScratchBuffer :: NvimE e m => Window -> String -> m Buffer
setupScratchBuffer window name = do
  buffer <- windowGetBuffer window
  configureScratchBuffer buffer name
  return buffer

createScratch :: NvimE e m => ScratchOptions -> m Scratch
createScratch (ScratchOptions useTab vertical size wrap name) = do
  (window, tab) <- createScratchUi useTab vertical wrap size
  buffer <- setupScratchBuffer window name
  return (Scratch buffer window tab)

setScratchContent :: NvimE e m => Scratch -> [String] -> m ()
setScratchContent (Scratch buffer _ _) lines' = do
  () <- bufferSetOption buffer "modifiable" (toMsgpack True)
  setBufferContent buffer lines'
  bufferSetOption buffer "modifiable" (toMsgpack False)

showInScratch :: NvimE e m => [String] -> ScratchOptions -> m Scratch
showInScratch lines' options = do
  scratch <- createScratch options
  setScratchContent scratch lines'
  return scratch
