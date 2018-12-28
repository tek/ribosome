module Ribosome.Scratch(
  showInScratch,
) where

import Neovim (
  Neovim,
  Buffer,
  Window,
  Tabpage,
  toObject,
  vim_command',
  vim_get_current_window',
  vim_get_current_tabpage',
  buffer_set_option',
  buffer_set_name',
  window_get_buffer',
  window_set_option',
  )
import Ribosome.Data.Scratch (Scratch(Scratch))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Api.Buffer (setBufferContent)

createScratchTab :: Neovim e Tabpage
createScratchTab = do
  vim_command' "tabnew"
  vim_get_current_tabpage'

createScratchWindow :: Bool -> Bool -> Maybe Int -> Neovim e Window
createScratchWindow vertical wrap size = do
  vim_command' $ prefix ++ cmd
  win <- vim_get_current_window'
  window_set_option' win "wrap" (toObject wrap)
  return win
  where
    cmd = if vertical then "vnew" else "new"
    prefix = maybe "" show size

createScratchUiInTab :: Neovim e (Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vim_get_current_window'
  return (win, Just tab)

createScratchUiInWindow :: Bool -> Bool -> Maybe Int -> Neovim e (Window, Maybe Tabpage)
createScratchUiInWindow vertical wrap size = do
  win <- createScratchWindow vertical wrap size
  return (win, Nothing)

createScratchUi :: Bool -> Bool -> Bool -> Maybe Int -> Neovim e (Window, Maybe Tabpage)
createScratchUi True _ _ _ =
  createScratchUiInTab
createScratchUi False vertical wrap size =
  createScratchUiInWindow vertical wrap size

configureScratchBuffer :: Buffer -> String -> Neovim e ()
configureScratchBuffer buffer name = do
  buffer_set_option' buffer "buftype" (toObject "nofile")
  buffer_set_option' buffer "bufhidden" (toObject "wipe")
  buffer_set_name' buffer name

setupScratchBuffer :: Window -> String -> Neovim e Buffer
setupScratchBuffer window name = do
  buffer <- window_get_buffer' window
  configureScratchBuffer buffer name
  return buffer

createScratch :: ScratchOptions -> Neovim e Scratch
createScratch (ScratchOptions useTab vertical size wrap name) = do
  (window, tab) <- createScratchUi useTab vertical wrap size
  buffer <- setupScratchBuffer window name
  return (Scratch buffer window tab)

setScratchContent :: Scratch -> [String] -> Neovim e ()
setScratchContent (Scratch buffer _ _) lines' = do
  buffer_set_option' buffer "modifiable" (toObject True)
  setBufferContent buffer lines'
  buffer_set_option' buffer "modifiable" (toObject False)

showInScratch :: [String] -> ScratchOptions -> Neovim e Scratch
showInScratch lines' options = do
  scratch <- createScratch options
  setScratchContent scratch lines'
  return scratch
