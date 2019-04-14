module Ribosome.Api.Window where

import Control.Monad (when)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimGetCurrentWin, nvimWinClose, nvimWinGetCursor, nvimWinSetCursor, windowIsValid)

closeWindow ::
  NvimE e m =>
  Window ->
  m ()
closeWindow window = do
  valid <- windowIsValid window
  when valid $ nvimWinClose window True

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

currentLine ::
  NvimE e m =>
  m Int
currentLine =
  fst <$> (cursor =<< nvimGetCurrentWin)

setCursor ::
  NvimE e m =>
  Window ->
  Int ->
  Int ->
  m ()
setCursor window line col =
  nvimWinSetCursor window (line + 1, col)
