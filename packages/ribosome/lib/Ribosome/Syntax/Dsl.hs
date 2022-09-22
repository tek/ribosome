-- |DSL for Neovim syntax definitions.
module Ribosome.Syntax.Dsl where

import Prelude hiding (group)

import Ribosome.Data.Syntax.Dsl (Alg (..))
import qualified Ribosome.Data.Syntax.SyntaxKind as SyntaxKind
import Ribosome.Data.Syntax.SyntaxKind (SyntaxKind (Match, Region, Verbatim), SyntaxRegion (SyntaxRegion))
import Ribosome.Data.SyntaxItem (SyntaxGroup, SyntaxItem (SyntaxItem))

-- |Declare the right syntax tree to be contained within the left tree, meaning that the top level nodes in the right
-- tree can only match inside of the nodes in the left tree.
--
-- On the left side, this sets the @contains@ option.
-- It propagates through all choice nodes and down to the innermost node in a chain (@contains@ or @nextgroup@).
--
-- On the right side, this sets the @contained@ option on the immediate child nodes.
(#>) :: Alg -> Alg -> Alg
(#>) =
  Contain

infixr 5 #>

-- |Declare that the right syntax tree should be matched immediately after the left tree.
--
-- On the left side, this sets the @nextgroup@ option.
-- It propagates through all choice nodes and down to the innermost node in a chain (@contains@ or @nextgroup@).
(>-) :: Alg -> Alg -> Alg
(>-) =
  Chain

infixr 6 >-

-- |Set a prefix for all syntax groups in the subtree.
--
-- In @prefix "Ribosome" (prefix "Long" (match "Word" "..."))@, the inner @match@ will have the group name
-- @RibosomeLongWord@.
--
-- Propagates through all node types.
prefix :: SyntaxGroup -> Alg -> Alg
prefix =
  Prefix

-- |Construct a concrete syntax item.
item :: SyntaxGroup -> SyntaxKind -> Alg
item grp k =
  Item (SyntaxItem grp k [] [] [] [] False)

-- |Construct a @syntax match@ item.
match :: SyntaxGroup -> Text -> Alg
match grp pat =
  item grp (Match pat)

-- |Construct a @syntax region@ item.
region :: SyntaxGroup -> Text -> Text -> Alg
region grp start end =
  item grp (Region SyntaxRegion {skip = Nothing, startOffset = Nothing, endOffset = Nothing, ..})

-- |Construct an item with an explicit syntax command that is not subject to transformations from other combinators.
verbatim :: Text -> Alg
verbatim =
  item "verbatim" . Verbatim

-- |Modify all nested syntax items with the specified function.
modItem :: (SyntaxItem -> SyntaxItem) -> Alg -> Alg
modItem =
  Mod

-- |Modify all nested syntax items with the specified function, operating on the 'SyntaxKind'.
modKind :: (SyntaxKind -> SyntaxKind) -> Alg -> Alg
modKind f =
  modItem (#kind %~ f)

-- |Set the @skip@ attribute on all nested regions.
skip :: Text -> Alg -> Alg
skip spec =
  modKind \case
    Region r ->
      Region (r & #skip ?~ spec)
    k ->
      k

-- |Set the @startOffset@ attribute on all nested regions.
startOffset :: Text -> Alg -> Alg
startOffset spec =
  modKind \case
    Region r ->
      Region (r & #startOffset ?~ spec)
    k ->
      k

-- |Set the @endOffset@ attribute on all nested regions.
endOffset :: Text -> Alg -> Alg
endOffset spec =
  modKind \case
    Region r ->
      Region (r & #endOffset ?~ spec)
    k ->
      k

-- |Add a highlight group to all nodes in the subtree.
--
-- If the subtree contains more than one node, this creates one @highlight@ and uses @highlight link@ to apply it to the
-- other groups.
hi :: Map Text Text -> Alg -> Alg
hi =
  Hi

-- |Add a highlight link to all nodes in the subtree.
link :: SyntaxGroup -> Alg -> Alg
link =
  Link
