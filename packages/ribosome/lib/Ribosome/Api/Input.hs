module Ribosome.Api.Input where

import Conc (withAsync_)
import qualified Polysemy.Time as Time
import Time (MilliSeconds, NanoSeconds, convert)

import Ribosome.Host.Api.Effect (vimInput)
import Ribosome.Host.Effect.Rpc (Rpc)

syntheticInput ::
  Members [Rpc, Time t d] r =>
  Maybe NanoSeconds ->
  [Text] ->
  Sem r ()
syntheticInput interval =
  traverse_ \ c ->
    traverse_ Time.sleep interval *> vimInput c

withInput ::
  Members [Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withInput delay interval chrs =
  withAsync_ (traverse_ Time.sleep delay *> syntheticInput (convert <$> interval) chrs)
