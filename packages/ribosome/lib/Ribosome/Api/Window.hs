-- |API functions for windows.
module Ribosome.Api.Window where

import Ribosome.Data.WindowView (PartialWindowView, WindowView)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (
  nvimBufGetOption,
  nvimCallFunction,
  nvimCommand,
  nvimGetCurrentWin,
  nvimWinClose,
  nvimWinGetBuf,
  nvimWinGetCursor,
  nvimWinSetCursor,
  vimCallFunction,
  vimGetWindows,
  vimSetCurrentWindow,
  windowIsValid,
  )
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (silentBang)

-- |Close a window if it is valid and not the last one.
closeWindow ::
  Member Rpc r =>
  Window ->
  Sem r ()
closeWindow window = do
  valid <- windowIsValid window
  last' <- (1 ==) . length <$> vimGetWindows
  when (valid && not last') $ nvimWinClose window True

-- |Get the zero-based position of the cursor in a window.
cursor ::
  Member Rpc r =>
  Window ->
  Sem r (Int, Int)
cursor window = do
  (line, col) <- nvimWinGetCursor window
  pure (line - 1, col)

-- |Get the zero-based position of the cursor in the active window.
currentCursor ::
  Member Rpc r =>
  Sem r (Int, Int)
currentCursor =
  cursor =<< nvimGetCurrentWin

-- |Get the zero-based line number of the cursor in a window.
windowLine ::
  Member Rpc r =>
  Window ->
  Sem r Int
windowLine window =
  fst <$> cursor window

-- |Get the zero-based line number of the cursor in the active window.
currentLine ::
  Member Rpc r =>
  Sem r Int
currentLine =
  windowLine =<< nvimGetCurrentWin

-- |Set the zero-based position of the cursor in a window.
setCursor ::
  Member Rpc r =>
  Window ->
  Int ->
  Int ->
  Sem r ()
setCursor window line col =
  nvimWinSetCursor window (line + 1, col)

-- |Set the zero-based position of the cursor in the current window.
setCurrentCursor ::
  Member Rpc r =>
  Int ->
  Int ->
  Sem r ()
setCurrentCursor line col = do
  window <- nvimGetCurrentWin
  setCursor window line col

-- |Set the zero-based line number of the cursor in a window, using the beginning of the line for the column.
setLine ::
  Member Rpc r =>
  Window ->
  Int ->
  Sem r ()
setLine window line =
  setCursor window line 0

-- |Set the zero-based line number of the cursor in the current window, using the beginning of the line for the column.
setCurrentLine ::
  Member Rpc r =>
  Int ->
  Sem r ()
setCurrentLine line =
  setCurrentCursor line 0

-- |Redraw the screen.
redraw ::
  Member Rpc r =>
  Sem r ()
redraw =
  silentBang do
    nvimCommand "redraw!"

-- |A main window means here any non-window that may be used to edit a file, i.e. one with an empty 'buftype'.
findMainWindow ::
  Member Rpc r =>
  Sem r (Maybe Window)
findMainWindow =
  listToMaybe <$> (filterM isFile =<< vimGetWindows)
  where
    isFile w = do
      buf <- nvimWinGetBuf w
      (("" :: Text) ==) <$> nvimBufGetOption buf "buftype"

-- |Create a new window at the top if no existing window has empty 'buftype'.
-- Focuses the window.
ensureMainWindow ::
  Member Rpc r =>
  Sem r Window
ensureMainWindow =
  maybe create focus =<< findMainWindow
  where
    create = do
      nvimCommand "aboveleft new"
      nvimGetCurrentWin <* nvimCommand "wincmd K"
    focus w =
      w <$ vimSetCurrentWindow w

-- |Call @winsaveview@.
saveView ::
  Member Rpc r =>
  Sem r WindowView
saveView =
  vimCallFunction "winsaveview" []

-- |Call @winrestview@ with a previously obtained view from 'saveView'.
restoreView ::
  Member Rpc r =>
  PartialWindowView ->
  Sem r ()
restoreView v =
  vimCallFunction "winrestview" [toMsgpack v]

-- |Execute a command in a window.
windowExec ::
  Member Rpc r =>
  Window ->
  Text ->
  Sem r ()
windowExec window cmd =
  nvimCallFunction "win_execute" [toMsgpack window, toMsgpack cmd]
