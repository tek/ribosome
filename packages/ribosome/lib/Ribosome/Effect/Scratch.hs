{-# options_haddock prune #-}

-- |Scratch buffers
module Ribosome.Effect.Scratch where

import Prelude hiding (show)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.ScratchState (ScratchState)

-- |This effect manages scratch buffers, that is, transient buffers displaying text not associated with a file.
-- See 'ScratchOptions' for configuration.
data Scratch :: Effect where
  -- |Open a new scratch buffer and set its content to the supplied text.
  Show :: Foldable t => t Text -> ScratchOptions -> Scratch m ScratchState
  -- |Find a previously defined scratch buffer, ensure it is open and set its content to the supplied text.
  Update :: Foldable t => ScratchId -> t Text -> Scratch m ScratchState
  -- |Close a scratch buffer.
  Delete :: ScratchId -> Scratch m ()
  -- |Return the state of all currently managed scratch buffers.
  Get :: Scratch m [ScratchState]
  -- |Look up a scratch buffer by its ID.
  Find :: ScratchId -> Scratch m (Maybe ScratchState)

makeSem ''Scratch

-- |Create an empty scratch buffer.
open ::
  Member Scratch r =>
  ScratchOptions ->
  Sem r ScratchState
open =
  show @_ @[] mempty
