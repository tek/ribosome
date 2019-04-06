module Ribosome.Api.Path where

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCallFunction)

nvimCwd :: NvimE e m => m FilePath
nvimCwd = vimCallFunction "getcwd" []
