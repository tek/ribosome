module Ribosome.Api.Function(
  callFunction,
) where

import Neovim (Neovim, fromObject', NvimObject, Object, vim_call_function')

callFunction :: NvimObject a => String -> [Object] -> Neovim e a
callFunction name args = vim_call_function' name args >>= fromObject'
