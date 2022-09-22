-- |Constructors for syntax items, internal
module Ribosome.Syntax.Cons where

import qualified Data.Map.Strict as Map

import Ribosome.Data.Syntax.Syntax (Highlight (Highlight))
import Ribosome.Data.Syntax.SyntaxKind (SyntaxKind (..), SyntaxRegion (SyntaxRegion))
import Ribosome.Data.SyntaxItem (SyntaxGroup, SyntaxItem (..))

-- |Construct a default 'SyntaxItem' from a 'SyntaxKind'.
syntaxItem :: SyntaxGroup -> SyntaxKind -> SyntaxItem
syntaxItem grp detail =
  SyntaxItem grp detail def def [] [] False

-- |Construct a simple keyword syntax item.
syntaxKeywords :: SyntaxGroup -> NonEmpty Text -> SyntaxItem
syntaxKeywords grp keywords =
  syntaxItem grp (Keyword keywords)

-- |Construct a simple keyword syntax item with a single keyword.
syntaxKeyword :: SyntaxGroup -> Text -> SyntaxItem
syntaxKeyword grp =
  syntaxKeywords grp . pure

-- |Construct a simple match syntax item.
syntaxMatch :: SyntaxGroup -> Text -> SyntaxItem
syntaxMatch grp pat =
  syntaxItem grp (Match pat)

-- |Construct a region syntax item with offsets.
syntaxRegionOffset :: SyntaxGroup -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> SyntaxItem
syntaxRegionOffset grp start end skip ms me =
  syntaxItem grp (Region (SyntaxRegion start end skip ms me))

-- |Construct a simple region syntax item.
syntaxRegion :: SyntaxGroup -> Text -> Text -> Maybe Text -> SyntaxItem
syntaxRegion grp start end skip =
  syntaxRegionOffset grp start end skip Nothing Nothing

-- |Construct a simple verbatim syntax item.
syntaxVerbatim :: Text -> SyntaxItem
syntaxVerbatim =
  syntaxItem "verbatim" . Verbatim

-- |Construct a syntax highlight.
syntaxHighlight :: SyntaxGroup -> [(Text, Text)] -> Highlight
syntaxHighlight grp =
  Highlight grp . Map.fromList
