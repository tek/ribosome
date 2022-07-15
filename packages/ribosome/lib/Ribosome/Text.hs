module Ribosome.Text where

import Data.Char (toUpper)
import qualified Data.Text as Text

escapeQuote :: Char -> Text
escapeQuote = \case
  '\'' ->
    "''"
  a ->
    Text.singleton a

escapeQuotes :: Text -> Text
escapeQuotes =
  Text.concatMap escapeQuote

capitalize :: Text -> Text
capitalize a =
  maybe "" run (Text.uncons a)
  where
    run (h, t) =
      Text.cons (toUpper h) t
