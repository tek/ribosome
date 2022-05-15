module Ribosome.Api.Input where

import Ribosome.Host.Api.Effect (vimInput)
import Ribosome.System.Time (sleep)

syntheticInput ::
  MonadIO m =>
  Member Rpc r =>
  Maybe Double ->
  [Text] ->
  m ()
syntheticInput interval =
  traverse_ send
  where
    send c =
      traverse_ sleep interval *> vimInput c
