module Ribosome.Scratch where

import Control.Monad (unless)
import Data.Default (Default(def))
import Data.Foldable (traverse_)

import Ribosome.Api.Buffer (setBufferContent)
import Ribosome.Api.Syntax (executeWindowSyntax)
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
  vimSetCurrentWindow,
  windowGetBuffer,
  windowSetOption,
  )

createScratchTab :: NvimE e m => m Tabpage
createScratchTab = do
  vimCommand "tabnew"
  vimGetCurrentTabpage

createScratchWindow :: NvimE e m => Bool -> Bool -> Maybe Int -> m Window
createScratchWindow vertical wrap size = do
  vimCommand $ prefix ++ cmd
  win <- vimGetCurrentWindow
  windowSetOption win "wrap" (toMsgpack wrap)
  return win
  where
    cmd = if vertical then "vnew" else "new"
    prefix = maybe "" show size

createScratchUiInTab :: NvimE e m => m (Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  return (win, Just tab)

createScratchUiInWindow :: NvimE e m => Bool -> Bool -> Bool -> Maybe Int -> m (Window, Maybe Tabpage)
createScratchUiInWindow vertical wrap focus size = do
  previous <- vimGetCurrentWindow
  win <- createScratchWindow vertical wrap size
  unless focus $ vimSetCurrentWindow previous
  return (win, Nothing)

createScratchUi :: NvimE e m => Bool -> Bool -> Bool -> Bool -> Maybe Int -> m (Window, Maybe Tabpage)
createScratchUi True _ _ _ _ =
  createScratchUiInTab
createScratchUi False vertical wrap focus size =
  createScratchUiInWindow vertical wrap focus size

configureScratchBuffer :: NvimE e m => Buffer -> String -> m ()
configureScratchBuffer buffer name = do
  bufferSetOption buffer "buftype" (toMsgpack ("nofile" :: String))
  bufferSetOption buffer "bufhidden" (toMsgpack ("wipe" :: String))
  bufferSetName buffer name

setupScratchBuffer :: NvimE e m => Window -> String -> m Buffer
setupScratchBuffer window name = do
  buffer <- windowGetBuffer window
  configureScratchBuffer buffer name
  return buffer

createScratch :: NvimE e m => ScratchOptions -> m Scratch
createScratch (ScratchOptions useTab vertical wrap focus size syntax name) = do
  (window, tab) <- createScratchUi useTab vertical wrap focus size
  buffer <- setupScratchBuffer window name
  traverse_ (executeWindowSyntax window) syntax
  return (Scratch buffer window tab)

setScratchContent :: NvimE e m => Scratch -> [String] -> m ()
setScratchContent (Scratch buffer _ _) lines' = do
  bufferSetOption buffer "modifiable" (toMsgpack True)
  setBufferContent buffer lines'
  bufferSetOption buffer "modifiable" (toMsgpack False)

showInScratch :: NvimE e m => [String] -> ScratchOptions -> m Scratch
showInScratch lines' options = do
  scratch <- createScratch options
  setScratchContent scratch lines'
  return scratch

showInScratchDef :: NvimE e m => [String] -> m Scratch
showInScratchDef lines' =
  showInScratch lines' def

showInScratchOrCreate :: NvimE e m => Maybe Scratch -> [String] -> ScratchOptions -> m Scratch
showInScratchOrCreate (Just scratch) lines' _ =
  scratch <$ setScratchContent scratch lines'
showInScratchOrCreate Nothing lines' options =
  showInScratch lines' options

showInScratchOrCreateDef :: NvimE e m => Maybe Scratch -> [String] -> m Scratch
showInScratchOrCreateDef scratch lines' =
  showInScratchOrCreate scratch lines' def
