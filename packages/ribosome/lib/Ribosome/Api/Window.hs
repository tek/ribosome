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

closeWindow ::
  Member Rpc r =>
  Window ->
  Sem r ()
closeWindow window = do
  valid <- windowIsValid window
  last' <- (1 ==) . length <$> vimGetWindows
  when (valid && not last') $ nvimWinClose window True

cursor ::
  Member Rpc r =>
  Window ->
  Sem r (Int, Int)
cursor window = do
  (line, col) <- nvimWinGetCursor window
  pure (line - 1, col)

currentCursor ::
  Member Rpc r =>
  Sem r (Int, Int)
currentCursor =
  cursor =<< nvimGetCurrentWin

windowLine ::
  Member Rpc r =>
  Window ->
  Sem r Int
windowLine window =
  fst <$> cursor window

currentLine ::
  Member Rpc r =>
  Sem r Int
currentLine =
  windowLine =<< nvimGetCurrentWin

setCursor ::
  Member Rpc r =>
  Window ->
  Int ->
  Int ->
  Sem r ()
setCursor window line col =
  nvimWinSetCursor window (line + 1, col)

setCurrentCursor ::
  Member Rpc r =>
  Int ->
  Int ->
  Sem r ()
setCurrentCursor line col = do
  window <- nvimGetCurrentWin
  setCursor window line col

setLine ::
  Member Rpc r =>
  Window ->
  Int ->
  Sem r ()
setLine window line =
  setCursor window line 0

setCurrentLine ::
  Member Rpc r =>
  Int ->
  Sem r ()
setCurrentLine line =
  setCurrentCursor line 0

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

saveView ::
  Member Rpc r =>
  Sem r WindowView
saveView =
  vimCallFunction "winsaveview" []

restoreView ::
  Member Rpc r =>
  PartialWindowView ->
  Sem r ()
restoreView v =
  vimCallFunction "winrestview" [toMsgpack v]

windowExec ::
  Member Rpc r =>
  Window ->
  Text ->
  Sem r ()
windowExec window cmd =
  nvimCallFunction "win_execute" [toMsgpack window, toMsgpack cmd]
