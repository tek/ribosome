module Ribosome.Data.Syntax where

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

data Highlight =
  Highlight {
    hiGroup :: Text,
    hiValues :: Map Text Text
  }
  deriving stock (Eq, Show)

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
