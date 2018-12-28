module Ribosome.Api.Option(
  optionCat,
  rtpCat,
  optionList,
  option,
  optionString,
) where

import Control.Monad ((>=>))
import Data.List.Split (splitOn)
import Neovim (Neovim, vim_get_option', fromObject', vim_set_option', toObject, NvimObject)

optionCat :: String -> String -> Neovim e ()
optionCat name extra = do
  current <- vim_get_option' name >>= fromObject'
  vim_set_option' name $ toObject $ current ++ "," ++ extra

rtpCat :: String -> Neovim e ()
rtpCat = optionCat "runtimepath"

option :: NvimObject a => String -> Neovim e a
option = vim_get_option' >=> fromObject'

optionString :: String -> Neovim e String
optionString = option

optionList :: String -> Neovim e [String]
optionList name = do
  s <- option name
  return $ splitOn "," s
