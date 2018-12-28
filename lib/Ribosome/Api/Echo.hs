module Ribosome.Api.Echo(
  echom,
  echomS,
) where

import Neovim (vim_command')
import Ribosome.Control.Ribo (Ribo)

escapeQuotes :: Char -> String
escapeQuotes '\'' = "''"
escapeQuotes a = [a]

echom :: String -> Ribo e ()
echom msg =
  vim_command' $ "echom '" ++ concatMap escapeQuotes msg ++ "'"

echomS :: Show a => a -> Ribo e ()
echomS = echom . show
