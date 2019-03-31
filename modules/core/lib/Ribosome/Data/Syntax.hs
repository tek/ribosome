module Ribosome.Data.Syntax where

import Data.Default (Default(def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map (fromList)

data SyntaxItem =
  Keyword {
    kwGroup :: String,
    kwKeyword :: String,
    kwOptions :: [String],
    kwKeywords :: [String],
    kwParams :: Map String String
    }
  |
  Match {
    matchGroup :: String,
    matchPattern :: String,
    kwOptions :: [String],
    kwParams :: Map String String
    }
  |
  Region {
    regionGroup :: String
    }
  deriving (Eq, Show)

syntaxKeyword :: String -> String -> SyntaxItem
syntaxKeyword group keyword =
  Keyword group keyword def def def

syntaxMatch :: String -> String -> SyntaxItem
syntaxMatch group pat =
  Match group pat def def

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
