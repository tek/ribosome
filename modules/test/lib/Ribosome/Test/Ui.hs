{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Ribosome.Test.Ui where

import Test.Framework

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (nvimListWins)

windowCountIs ::
  NvimE e m =>
  AssertM m =>
  Int ->
  m ()
windowCountIs count = do
  wins <- nvimListWins
  gassertEqual count (length wins)
