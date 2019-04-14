{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Ribosome.Test.Ui where

import Test.Framework

import Ribosome.Api.Window (currentCursor, cursor)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimListWins)

windowCountIs ::
  NvimE e m =>
  AssertM m =>
  Int ->
  m ()
windowCountIs count = do
  wins <- nvimListWins
  gassertEqual count (length wins)

cursorIs ::
  NvimE e m =>
  AssertM m =>
  Int ->
  Int ->
  Window ->
  m ()
cursorIs line col =
  gassertEqual (line, col) <=< cursor

currentCursorIs ::
  NvimE e m =>
  AssertM m =>
  Int ->
  Int ->
  m ()
currentCursorIs line col =
  gassertEqual (line, col) =<< currentCursor
