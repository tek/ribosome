-- |Combinators for 'Text'.
module Ribosome.Text where

import Data.Char (toUpper)
import qualified Data.Text as Text

-- |Escape a single quote Neovim-style by replacing it with two single quotes.
escapeQuote :: Char -> Text
escapeQuote = \case
  '\'' ->
    "''"
  a ->
    Text.singleton a

-- |Escape all single quotes Neovim-style by replacing them with two single quotes.
escapeQuotes :: Text -> Text
escapeQuotes =
  Text.concatMap escapeQuote

-- |Upcase the first letter of a 'Text', if any.
capitalize :: Text -> Text
capitalize a =
  maybe "" run (Text.uncons a)
  where
    run (h, t) =
      Text.cons (toUpper h) t
