-- |Data types for the Neovim syntax API.
module Ribosome.Data.Syntax.Syntax where

import Ribosome.Data.SyntaxItem (SyntaxGroup, SyntaxItem)

-- |Options for a highlight group.
data Highlight =
  Highlight {
    group :: SyntaxGroup,
    values :: Map Text Text
  }
  deriving stock (Eq, Show, Generic)

-- |Options for a @:highlight link@ command.
data HiLink =
  HiLink {
    group :: SyntaxGroup,
    target :: SyntaxGroup
  }
  deriving stock (Eq, Show, Generic)

-- |A set of syntax settings, consisting of syntax items like @keyword@ and @match@, highlights and highlight links.
data Syntax =
  Syntax {
     items :: [SyntaxItem],
     highlights :: [Highlight],
     links :: [HiLink]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
