module Ribosome.Api.Buffer(
  edit,
  buflisted,
  setBufferContent,
  bufferContent,
  currentBufferContent,
  setCurrentBufferContent,
) where

import Neovim (
  Neovim,
  Buffer,
  NvimObject(..),
  Object,
  toObject,
  vim_command',
  vim_call_function',
  buffer_get_number',
  buffer_get_lines',
  buffer_set_lines',
  vim_get_current_buffer',
  )
import Ribosome.Control.Ribo (Ribo)

edit :: FilePath -> Ribo e ()
edit path = vim_command' $ "silent! edit " ++ path

nvimCallBool :: String -> [Object] -> Neovim e Bool
nvimCallBool fun args =
  vim_call_function' fun args >>= fromObject'

buflisted :: Buffer -> Neovim e Bool
buflisted buf = do
  num <- buffer_get_number' buf
  nvimCallBool "buflisted" [toObject num]

bufferContent :: Buffer -> Neovim e [String]
bufferContent buffer =
  buffer_get_lines' buffer 0 (-1) False

currentBufferContent :: Neovim e [String]
currentBufferContent = do
  buffer <- vim_get_current_buffer'
  bufferContent buffer

setBufferContent :: Buffer -> [String] -> Neovim e ()
setBufferContent buffer =
  buffer_set_lines' buffer 0 (-1) False

setCurrentBufferContent :: [String] -> Neovim e ()
setCurrentBufferContent content = do
  buffer <- vim_get_current_buffer'
  setBufferContent buffer content
