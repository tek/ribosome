module Ribosome.Test.Ui where

import Hedgehog (TestT, (===))
import Ribosome.Api.Window (currentCursor, cursor)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimListWins)

windowCountIs ::
  NvimE e m =>
  Int ->
  TestT m ()
windowCountIs count = do
  wins <- nvimListWins
  count === (length wins)

cursorIs ::
  NvimE e m =>
  Int ->
  Int ->
  Window ->
  TestT m ()
cursorIs line col =
  ((line, col) ===) <=< lift . cursor

currentCursorIs ::
  NvimE e m =>
  Int ->
  Int ->
  TestT m ()
currentCursorIs line col =
  ((line, col) ===) =<< lift currentCursor
