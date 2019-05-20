module Ribosome.Api.Input where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimInput)
import Ribosome.System.Time (sleep)

syntheticInput ::
  MonadIO m =>
  NvimE e m =>
  Maybe Double ->
  [Text] ->
  m ()
syntheticInput interval =
  traverse_ send
  where
    send c =
      traverse_ sleep interval *> vimInput c
