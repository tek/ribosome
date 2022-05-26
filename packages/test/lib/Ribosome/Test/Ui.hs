module Ribosome.Test.Ui where

import Polysemy.Test (Hedgehog, assertEq, (===))

import Ribosome.Api.Window (currentCursor, cursor)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (nvimListWins)
import Ribosome.Host.Effect.Rpc (Rpc)

windowCountIs ::
  Monad m =>
  Members [Rpc, Hedgehog m] r =>
  Int ->
  Sem r ()
windowCountIs count = do
  wins <- nvimListWins
  count === length wins

cursorIs ::
  Monad m =>
  Members [Rpc, Hedgehog m] r =>
  Int ->
  Int ->
  Window ->
  Sem r ()
cursorIs line col =
  assertEq (line, col) <=< cursor

currentCursorIs ::
  Monad m =>
  Members [Rpc, Hedgehog m] r =>
  Int ->
  Int ->
  Sem r ()
currentCursorIs line col =
  assertEq (line, col) =<< currentCursor
