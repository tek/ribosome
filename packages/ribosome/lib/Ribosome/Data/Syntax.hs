module Ribosome.Data.Syntax where

import qualified Data.Map.Strict as Map (fromList)

data SyntaxItemDetail =
  Keyword {
    kwGroup :: Text,
    kwKeyword :: Text,
    kwOptions :: [Text]
  }
  |
  Match {
    matchGroup :: Text,
    matchPattern :: Text
  }
  |
  Region {
    regionGroup :: Text,
    regionStart :: Text,
    regionEnd :: Text,
    regionSkip :: Maybe Text,
    regionStartOffset :: Text,
    regionEndOffset :: Text
  }
  |
  Verbatim {
    verbatimCommand :: Text
  }
  deriving stock (Eq, Show)

data SyntaxItem =
  SyntaxItem {
    siDetail :: SyntaxItemDetail,
    siOptions :: [Text],
    siParams :: Map Text Text
  }
  deriving stock (Eq, Show)

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

data Highlight =
  Highlight {
    hiGroup :: Text,
    hiValues :: Map Text Text
  }
  deriving stock (Eq, Show)

syntaxHighlight :: Text -> [(Text, Text)] -> Highlight
syntaxHighlight group' =
  Highlight group' . Map.fromList

data HiLink =
  HiLink {
    hlGroup :: Text,
    hlTarget :: Text
  }
  deriving stock (Eq, Show)

data Syntax =
  Syntax {
     syntaxItems :: [SyntaxItem],
     syntaxHighlights :: [Highlight],
     syntaxHiLinks :: [HiLink]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
