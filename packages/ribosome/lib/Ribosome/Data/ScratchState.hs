-- |Scratch buffer state.
module Ribosome.Data.ScratchState where

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)

-- |The configuration and Neovim resources that define a scratch buffer and describe its previously recorded UI state.
data ScratchState =
  ScratchState {
    -- |The scratch buffer's ID stored in the state.
    id :: ScratchId,
    -- |The configuration used to create the scratch buffer.
    options :: ScratchOptions,
    -- |The Neovim buffer handle that was returned when it was last updated.
    buffer :: Buffer,
    -- |The Neovim window handle that was returned when it was last updated.
    window :: Window,
    -- |The Neovim window handle that denotes the window that was active when the scratch buffer was created.
    previous :: Window,
    -- |The Neovim tabpage handle that was returned when it was last updated, if a tab was requested by the
    -- configuration.
    tab :: Maybe Tabpage
  }
  deriving stock (Eq, Show, Generic)
