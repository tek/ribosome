-- |Data types for syntax keyword, match, region and verbatim code.
module Ribosome.Data.Syntax.SyntaxKind where

-- |A syntax region, defined by patterns for start and end, and optionally a skip pattern.
--
-- Offsets define precisely where highlighting begins.
--
-- See @:help :syn-region@.
data SyntaxRegion =
  SyntaxRegion {
    start :: Text,
    end :: Text,
    skip :: Maybe Text,
    startOffset :: Maybe Text,
    endOffset :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- |Data type for syntax keyword, match, region and verbatim code.
data SyntaxKind =
  -- |A list of precise tokens that should be matched.
  Keyword { keywords :: NonEmpty Text }
  |
  -- |A single pattern.
  Match { pattern_ :: Text }
  |
  -- |A region with start and end patterns.
  Region SyntaxRegion
  |
  -- |A command that is not processed by the DSL and inserted verbatim.
  --
  -- Useful for things like @syntax sync@.
  Verbatim { command :: Text }
  deriving stock (Eq, Show)
