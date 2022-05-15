module Ribosome.Test.Ui where

import Hedgehog (TestT, (===))
import Ribosome.Api.Window (currentCursor, cursor)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (nvimListWins)

windowCountIs ::
  Member Rpc r =>
  Int ->
  TestT m ()
windowCountIs count = do
  wins <- nvimListWins
  count === (length wins)

cursorIs ::
  Member Rpc r =>
  Int ->
  Int ->
  Window ->
  TestT m ()
cursorIs line col =
  ((line, col) ===) <=< lift . cursor

currentCursorIs ::
  Member Rpc r =>
  Int ->
  Int ->
  TestT m ()
currentCursorIs line col =
  ((line, col) ===) =<< lift currentCursor
