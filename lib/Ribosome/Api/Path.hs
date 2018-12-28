module Ribosome.Api.Path(
  nvimCwd,
) where

import Neovim (Neovim, vim_call_function', fromObject')

nvimCwd :: Neovim e FilePath
nvimCwd = vim_call_function' "getcwd" [] >>= fromObject'
