module Ribosome.Api.Option(
  optionCat,
  rtpCat,
  optionList,
  option,
  optionString,
) where

import Control.Monad ((>=>))
import Data.List.Split (splitOn)
import Neovim (Neovim, NvimObject, fromObject', toObject, vim_get_option', vim_set_option')

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimGetOption, vimSetOption)

optionCat :: NvimE e m => String -> String -> m ()
optionCat name extra = do
  current <- vimGetOption name
  vimSetOption name $ toObject $ current ++ "," ++ extra

rtpCat :: NvimE e m => String -> m ()
rtpCat = optionCat "runtimepath"

option :: NvimObject a => String -> Neovim e a
option = vim_get_option' >=> fromObject'

optionString :: String -> Neovim e String
optionString = option

optionList :: String -> Neovim e [String]
optionList name = do
  s <- option name
  return $ splitOn "," s
