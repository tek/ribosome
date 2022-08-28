-- |Data types and combinators for [Ribosome.Api.Syntax]("Ribosome.Api.Syntax").
module Ribosome.Syntax (
  module Ribosome.Data.Syntax,
  module Ribosome.Syntax,
) where

import qualified Data.Map.Strict as Map

import Ribosome.Data.Syntax (
  HiLink (..),
  Highlight (..),
  Syntax (..),
  SyntaxItem (..),
  SyntaxItemDetail (..),
  )

-- |Construct a default 'SyntaxItem' from a 'SyntaxItemDetail'.
syntaxItem :: SyntaxItemDetail -> SyntaxItem
syntaxItem detail =
  SyntaxItem detail def def

-- |Construct a simple keyword syntax item.
syntaxKeyword :: Text -> Text -> SyntaxItem
syntaxKeyword group' keyword =
  syntaxItem $ Keyword group' keyword def

-- |Construct a simple match syntax item.
syntaxMatch :: Text -> Text -> SyntaxItem
syntaxMatch group' pat =
  syntaxItem $ Match group' pat

-- |Construct a region syntax item with offsets.
syntaxRegionOffset :: Text -> Text -> Text -> Maybe Text -> Text -> Text -> SyntaxItem
syntaxRegionOffset group' start end skip ms me =
  syntaxItem $ Region group' start end skip ms me

-- |Construct a simple region syntax item.
syntaxRegion :: Text -> Text -> Text -> Maybe Text -> SyntaxItem
syntaxRegion group' start end skip =
  syntaxRegionOffset group' start end skip "" ""

-- |Construct a simple verbatim syntax item.
syntaxVerbatim :: Text -> SyntaxItem
syntaxVerbatim =
  syntaxItem . Verbatim

-- |Construct a syntax highlight.
syntaxHighlight :: Text -> [(Text, Text)] -> Highlight
syntaxHighlight group' =
  Highlight group' . Map.fromList
