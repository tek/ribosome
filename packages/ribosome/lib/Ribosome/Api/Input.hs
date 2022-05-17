module Ribosome.Api.Input where

import qualified Polysemy.Time as Time
import Time (NanoSeconds)

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
