module Ribosome.Effect.Scratch where

import Prelude hiding (show)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.ScratchState (ScratchState)

data Scratch :: Effect where
  Show :: Foldable t => t Text -> ScratchOptions -> Scratch m ScratchState
  Update :: Foldable t => ScratchId -> t Text -> Scratch m ScratchState
  Kill :: ScratchId -> Scratch m ()
  Get :: Scratch m [ScratchState]
  Find :: ScratchId -> Scratch m (Maybe ScratchState)

makeSem ''Scratch

open ::
  Member Scratch r =>
  ScratchOptions ->
  Sem r ScratchState
open =
  show @_ @[] mempty
