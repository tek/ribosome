module Ribosome.Data.String(
  escapeQuotes,
) where

escapeQuotes :: Char -> String
escapeQuotes '\'' = "''"
escapeQuotes a = [a]
