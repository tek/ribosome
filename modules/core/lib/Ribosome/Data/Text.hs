module Ribosome.Data.Text where

import Data.Char (toUpper)
import qualified Data.Text as Text (concatMap, cons, singleton, uncons)

escapeQuote :: Char -> Text
escapeQuote '\'' = "''"
escapeQuote a = Text.singleton a

escapeQuotes :: Text -> Text
escapeQuotes = Text.concatMap escapeQuote

capitalize :: Text -> Text
capitalize a =
  maybe "" run (Text.uncons a)
  where
    run (h, t) = Text.cons (toUpper h) t
