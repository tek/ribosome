module Ribosome.Effect.Scratch where

import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.ScratchState (ScratchId, ScratchState)

data Scratch :: Effect where
  Show :: Foldable t => t Text -> ScratchOptions -> Scratch m ScratchState
  Update :: ScratchId -> ScratchOptions -> Scratch m ScratchState
  Kill :: ScratchId -> Scratch m ()
  Get :: Scratch m [ScratchState]
  Find :: ScratchId -> Scratch m (Maybe ScratchState)

makeSem ''Scratch
