-- |Scratch buffer state.
module Ribosome.Data.ScratchState where

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Host.Data.RpcType (AutocmdId)

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
    tab :: Maybe Tabpage,
    -- |The ID of the autocmd that fires when the user deletes the scratch buffer.
    autocmdId :: AutocmdId
  }
  deriving stock (Eq, Show, Generic)
