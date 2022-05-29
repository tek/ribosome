module Ribosome.Data.ScratchState where

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)

data ScratchState =
  ScratchState {
    id :: ScratchId,
    options :: ScratchOptions,
    buffer :: Buffer,
    window :: Window,
    previous :: Window,
    tab :: Maybe Tabpage
  }
  deriving stock (Eq, Show, Generic)
