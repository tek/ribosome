module Ribosome.Api.Echo(
  echom,
  echomS,
) where

import Neovim (vim_command')
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (name)

escapeQuotes :: Char -> String
escapeQuotes '\'' = "''"
escapeQuotes a = [a]

echom :: String -> Ribo e ()
echom msg = do
  name <- Ribo.name
  vim_command' $ "echom '" ++ name ++ ": " ++ concatMap escapeQuotes msg ++ "'"

echomS :: Show a => a -> Ribo e ()
echomS = echom . show
