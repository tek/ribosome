-- |Data types for syntax items.
module Ribosome.Data.SyntaxItem where

import Ribosome.Data.Syntax.SyntaxKind (SyntaxKind)

-- |The identifier for a syntax item.
newtype SyntaxGroup =
  SyntaxGroup { unSyntaxGroup :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, Semigroup, Monoid)

-- |A syntax item like @keyword@ or @match@, bundled with options for the @:syntax@ command.
data SyntaxItem =
  SyntaxItem {
    group :: SyntaxGroup,
    kind :: SyntaxKind,
    options :: [Text],
    params :: Map Text Text,
    next :: [SyntaxGroup],
    contains :: [SyntaxGroup],
    contained :: Bool
  }
  deriving stock (Eq, Show, Generic)
