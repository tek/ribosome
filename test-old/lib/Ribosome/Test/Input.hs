module Ribosome.Test.Input where

import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)

import Ribosome.Api.Input (syntheticInput)

withInput ::
  Member Rpc r =>
  MonadIO m =>
  Maybe Double ->
  [Text] ->
  m a ->
  m a
withInput interval chars thunk =
  bracket (fork input) killThread (const thunk)
  where
    input =
      syntheticInput interval chars
