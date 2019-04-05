module Ribosome.Data.Syntax where

import Data.Default (Default(def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map (fromList)

data SyntaxItemDetail =
  Keyword {
    kwGroup :: String,
    kwKeyword :: String,
    kwOptions :: [String]
  }
  |
  Match {
    matchGroup :: String,
    matchPattern :: String
  }
  |
  Region {
    regionGroup :: String,
    regionStart :: String,
    regionEnd :: String,
    regionSkip :: Maybe String
  }
  |
  Verbatim {
    verbatimCommand :: String
  }
  deriving (Eq, Show)

data SyntaxItem =
  SyntaxItem {
    siDetail :: SyntaxItemDetail,
    siOptions :: [String],
    siParams :: Map String String
  }
  deriving (Eq, Show)

syntaxItem :: SyntaxItemDetail -> SyntaxItem
syntaxItem detail =
  SyntaxItem detail def def

syntaxKeyword :: String -> String -> SyntaxItem
syntaxKeyword group keyword =
  syntaxItem $ Keyword group keyword def

syntaxMatch :: String -> String -> SyntaxItem
syntaxMatch group pat =
  syntaxItem $ Match group pat

syntaxRegion :: String -> String -> String -> Maybe String -> SyntaxItem
syntaxRegion group start end skip =
  syntaxItem $ Region group start end skip

syntaxVerbatim :: String -> SyntaxItem
syntaxVerbatim =
  syntaxItem . Verbatim

data Highlight =
  Highlight {
    hiGroup :: String,
    hiValues :: Map String String
  }
  deriving (Eq, Show)

syntaxHighlight :: String -> [(String, String)] -> Highlight
syntaxHighlight group =
  Highlight group . Map.fromList

data HiLink =
  HiLink {
    hlGroup :: String,
    hlTarget :: String
  }
  deriving (Eq, Show)

data Syntax =
  Syntax {
     syntaxItems :: [SyntaxItem],
     syntaxHighlights :: [Highlight],
     syntaxHiLinks :: [HiLink]
  }
  deriving (Eq, Show)
