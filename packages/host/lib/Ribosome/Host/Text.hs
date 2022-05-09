module Ribosome.Host.Text where

import qualified Data.Text as Text
import Exon (exon)

ellipsize :: Int -> Text -> Text
ellipsize maxChars msg =
  [exon|#{pre}#{if Text.null post then "" else "..."}|]
  where
    (pre, post) =
      Text.splitAt maxChars msg
