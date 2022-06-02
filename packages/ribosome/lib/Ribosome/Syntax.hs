module Ribosome.Syntax (
  module Ribosome.Syntax,
  module Ribosome.Data.Syntax,
) where

import qualified Data.Map.Strict as Map

import Ribosome.Data.Syntax (
  HiLink (..),
  Highlight (..),
  Syntax (..),
  SyntaxItem (..),
  SyntaxItemDetail (..),
  )

syntaxItem :: SyntaxItemDetail -> SyntaxItem
syntaxItem detail =
  SyntaxItem detail def def

syntaxKeyword :: Text -> Text -> SyntaxItem
syntaxKeyword group' keyword =
  syntaxItem $ Keyword group' keyword def

syntaxMatch :: Text -> Text -> SyntaxItem
syntaxMatch group' pat =
  syntaxItem $ Match group' pat

syntaxRegionOffset :: Text -> Text -> Text -> Maybe Text -> Text -> Text -> SyntaxItem
syntaxRegionOffset group' start end skip ms me =
  syntaxItem $ Region group' start end skip ms me

syntaxRegion :: Text -> Text -> Text -> Maybe Text -> SyntaxItem
syntaxRegion group' start end skip =
  syntaxRegionOffset group' start end skip "" ""

syntaxVerbatim :: Text -> SyntaxItem
syntaxVerbatim =
  syntaxItem . Verbatim

syntaxHighlight :: Text -> [(Text, Text)] -> Highlight
syntaxHighlight group' =
  Highlight group' . Map.fromList
