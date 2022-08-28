-- |Functions for simulating user input in tests.
module Ribosome.Api.Input where

import Conc (withAsync_)
import qualified Polysemy.Time as Time
import Time (MilliSeconds, NanoSeconds, convert)

import Ribosome.Host.Api.Effect (nvimInput, nvimReplaceTermcodes, nvimFeedkeys)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Send a list of character sequences as user input to Neovim with an optional wait interval.
--
-- Uses @nvim_input@.
syntheticInput ::
  Members [Rpc, Time t d] r =>
  Maybe NanoSeconds ->
  [Text] ->
  Sem r ()
syntheticInput interval =
  traverse_ \ c ->
    traverse_ Time.sleep interval *> nvimInput c

-- |Send a sequence of keys using @nvim_feedkeys@ after replacing terminal codes.
feedKey ::
  Member Rpc r =>
  Text ->
  Sem r ()
feedKey k = do
  key <- nvimReplaceTermcodes k True False True
  nvimFeedkeys key "mt" False

-- |Send a list of character sequences as user input to Neovim with an optional wait interval.
--
-- Uses @nvim_feedkeys@.
syntheticInputFk ::
  Members [Rpc, Time t d] r =>
  Maybe NanoSeconds ->
  [Text] ->
  Sem r ()
syntheticInputFk interval =
  traverse_ \ c ->
    traverse_ Time.sleep interval *> feedKey c

-- |Run an action after forking a thread that sends user input to Neovim. 
withInput ::
  Members [Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withInput delay interval chrs =
  withAsync_ (traverse_ Time.sleep delay *> syntheticInput (convert <$> interval) chrs)
