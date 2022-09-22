{-# options_haddock prune #-}

-- |Internal combinators for syntax.
module Ribosome.Internal.Syntax where

import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Data.Syntax.Syntax (HiLink (HiLink), Highlight (Highlight), Syntax (Syntax))
import Ribosome.Data.Syntax.SyntaxKind (SyntaxKind (..), SyntaxRegion (SyntaxRegion))
import Ribosome.Data.SyntaxItem (SyntaxGroup (SyntaxGroup), SyntaxItem (SyntaxItem))
import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Data.RpcCall (RpcCall)

joinEquals :: Map Text Text -> Text
joinEquals =
  unwords . fmap equals . Map.toList
  where
    equals (a, b) =
      [exon|#{a}=#{b}|]

synPattern :: Text -> Text
synPattern pat =
  [exon|/#{pat}/|]

namedPattern :: Text -> Text -> Maybe Text -> Text
namedPattern param pat offset =
  [exon|#{param}=#{synPattern pat}#{fold offset}|]

syntaxItemDetailCmd :: SyntaxGroup -> SyntaxKind -> [Text]
syntaxItemDetailCmd (SyntaxGroup grp) = \case
  Keyword keywords ->
    ["syntax", "keyword", grp] ++ toList keywords
  Match pat ->
    ["syntax", "match", grp, synPattern pat]
  Region (SyntaxRegion start end skip ms me) ->
    ["syntax", "region", grp, namedPattern "start" start ms]
    <>
    foldMap skipArg skip
    <>
    [namedPattern "end" end me]
    where
      skipArg a = [namedPattern "skip" a Nothing]
  Verbatim cmd ->
    [cmd]

syntaxItemCmd :: SyntaxItem -> [Text]
syntaxItemCmd (SyntaxItem grp detail options params next contains contained) =
  syntaxItemDetailCmd grp detail <> [
    unwords (nubOrd (options <> containedOpt)),
    joinEquals (withParam (coerce <$> contains) "contains" (withParam (coerce <$> next) "nextgroup" params))
  ]
  where
    containedOpt =
      if contained then ["contained"] else []
    withParam val =
      if null val
      then const id
      else
      let commas = Text.intercalate "," val
      in Map.alter \case
        Just old -> Just [exon|#{old},#{commas}|]
        Nothing -> Just commas

highlightCmd :: Highlight -> [Text]
highlightCmd (Highlight (SyntaxGroup grp) values) =
  ["highlight", "default", grp, joinEquals values]

hilinkCmd :: HiLink -> [Text]
hilinkCmd (HiLink (SyntaxGroup grp) (SyntaxGroup target)) =
  ["highlight", "default", "link", grp, target]

syntaxCmds :: Syntax -> [[Text]]
syntaxCmds (Syntax items highlights hilinks) =
  (syntaxItemCmd <$> items) <> (highlightCmd <$> highlights) <> (hilinkCmd <$> hilinks)

catCmd :: [Text] -> RpcCall ()
catCmd =
  Data.nvimCommand . Text.unwords

catCmds :: [[Text]] -> RpcCall ()
catCmds =
  foldMap catCmd
