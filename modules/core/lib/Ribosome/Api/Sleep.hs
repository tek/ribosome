module Ribosome.Api.Sleep(
  nvimSleep,
  nvimMSleep,
) where

import Neovim (Neovim, vim_command')

nvimSleep :: Int -> Neovim e ()
nvimSleep interval = vim_command' $ "sleep " ++ show interval

nvimMSleep :: Int -> Neovim e ()
nvimMSleep interval = vim_command' $ "sleep " ++ show interval ++ "m"
