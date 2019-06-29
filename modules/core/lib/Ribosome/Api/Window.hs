module Ribosome.Api.Window where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (
  nvimBufGetOption,
  nvimGetCurrentWin,
  nvimWinClose,
  nvimWinGetBuf,
  nvimWinGetCursor,
  nvimWinSetCursor,
  vimCommand,
  vimGetWindows,
  vimSetCurrentWindow,
  windowIsValid,
  )

closeWindow ::
  NvimE e m =>
  Window ->
  m ()
closeWindow window = do
  valid <- windowIsValid window
  last' <- (1 ==) . length <$> vimGetWindows
  when (valid && not last') $ nvimWinClose window True

cursor ::
  NvimE e m =>
  Window ->
  m (Int, Int)
cursor window = do
  (line, col) <- nvimWinGetCursor window
  return (fromIntegral line - 1, fromIntegral col)

currentCursor ::
  NvimE e m =>
  m (Int, Int)
currentCursor =
  cursor =<< nvimGetCurrentWin

windowLine ::
  NvimE e m =>
  Window ->
  m Int
windowLine window =
  fst <$> cursor window

currentLine ::
  NvimE e m =>
  m Int
currentLine =
  windowLine =<< nvimGetCurrentWin

setCursor ::
  NvimE e m =>
  Window ->
  Int ->
  Int ->
  m ()
setCursor window line col =
  nvimWinSetCursor window (line + 1, col)

setCurrentCursor ::
  NvimE e m =>
  Int ->
  Int ->
  m ()
setCurrentCursor line col = do
  window <- nvimGetCurrentWin
  setCursor window line col

setLine ::
  NvimE e m =>
  Window ->
  Int ->
  m ()
setLine window line =
  setCursor window line 0

setCurrentLine ::
  NvimE e m =>
  Int ->
  m ()
setCurrentLine line =
  setCurrentCursor line 0

redraw ::
  NvimE e m =>
  m ()
redraw =
  vimCommand "silent! redraw!"

-- |A main window means here any non-window that may be used to edit a file, i.e. one with an empty 'buftype'.
findMainWindow ::
  NvimE e m =>
  m (Maybe Window)
findMainWindow =
  listToMaybe <$> (filterM isFile =<< vimGetWindows)
  where
    isFile w = do
      buf <- nvimWinGetBuf w
      (("" :: Text) ==) <$> nvimBufGetOption buf "buftype"

-- |Create a new window at the top if no existing window has empty 'buftype'.
-- Focuses the window.
ensureMainWindow ::
  NvimE e m =>
  m Window
ensureMainWindow =
  maybe create focus =<< findMainWindow
  where
    create = do
      vimCommand "aboveleft new"
      win <- nvimGetCurrentWin
      vimCommand "wincmd K"
      return win
    focus w =
      w <$ vimSetCurrentWindow w
