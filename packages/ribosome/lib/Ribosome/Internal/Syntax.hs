module Ribosome.Internal.Syntax where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Data.Syntax (
  HiLink (HiLink),
  Highlight (Highlight),
  Syntax (Syntax),
  SyntaxItem (SyntaxItem),
  SyntaxItemDetail (Keyword, Match, Region, Verbatim),
  )
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

namedPattern :: Text -> Text -> Text -> Text
namedPattern param pat offset =
  [exon|#{param}=#{synPattern pat}#{offset}|]

syntaxItemDetailCmd :: SyntaxItemDetail -> [Text]
syntaxItemDetailCmd (Keyword group' keyword keywords) =
  ["syntax", "keyword", group', keyword, unwords keywords]
syntaxItemDetailCmd (Match group' pat) =
  ["syntax", "match", group', synPattern pat]
syntaxItemDetailCmd (Region group' start end skip ms me) =
  ["syntax", "region", group', namedPattern "start" start ms] <> foldMap skipArg skip <> [namedPattern "end" end me]
  where
    skipArg a = [namedPattern "skip" a ""]
syntaxItemDetailCmd (Verbatim cmd) =
  [cmd]

syntaxItemCmd :: SyntaxItem -> [Text]
syntaxItemCmd (SyntaxItem detail options params) =
  syntaxItemDetailCmd detail <> [unwords options, joinEquals params]

highlightCmd :: Highlight -> [Text]
highlightCmd (Highlight group' values) =
  ["highlight", "default", group', joinEquals values]

hilinkCmd :: HiLink -> [Text]
hilinkCmd (HiLink group' target) =
  ["highlight", "default", "link", group', target]

syntaxCmds :: Syntax -> [[Text]]
syntaxCmds (Syntax items highlights hilinks) =
  (syntaxItemCmd <$> items) <> (highlightCmd <$> highlights) <> (hilinkCmd <$> hilinks)

catCmd :: [Text] -> RpcCall ()
catCmd =
  Data.nvimCommand . Text.unwords

catCmds :: [[Text]] -> RpcCall ()
catCmds =
  foldMap catCmd
