module Ribosome.Data.String where

import Data.Char (toUpper)

escapeQuotes :: Char -> String
escapeQuotes '\'' = "''"
escapeQuotes a = [a]

capitalize :: String -> String
capitalize [] = []
capitalize (head' : tail') = toUpper head' : tail'
