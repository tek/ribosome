-- |Data types for the Neovim syntax API.
module Ribosome.Data.Syntax where

-- |Different kinds of syntax items.
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

-- |A syntax item like @keyword@ or @match@, bundled with options for the @:syntax@ command.
data SyntaxItem =
  SyntaxItem {
    siDetail :: SyntaxItemDetail,
    siOptions :: [Text],
    siParams :: Map Text Text
  }
  deriving stock (Eq, Show)

-- |Options for a highlight group.
data Highlight =
  Highlight {
    hiGroup :: Text,
    hiValues :: Map Text Text
  }
  deriving stock (Eq, Show)

-- |Options for a @:highlight link@ command.
data HiLink =
  HiLink {
    hlGroup :: Text,
    hlTarget :: Text
  }
  deriving stock (Eq, Show)

-- |A set of syntax settings, consisting of syntax items like @keyword@ and @match@, highlights and highlight links.
data Syntax =
  Syntax {
     syntaxItems :: [SyntaxItem],
     syntaxHighlights :: [Highlight],
     syntaxHiLinks :: [HiLink]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
