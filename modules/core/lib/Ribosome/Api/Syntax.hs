module Ribosome.Api.Syntax where

import Data.Functor.Syntax ((<$$>))
import Data.Map (Map)
import qualified Data.Map as Map (toList)
import Data.MessagePack (Object)
import Neovim.Plugin.Classes (FunctionName(F))

import Ribosome.Api.Atomic (atomic)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Syntax (
  HiLink(HiLink),
  Highlight(Highlight),
  Syntax(Syntax),
  SyntaxItem(SyntaxItem),
  SyntaxItemDetail(Keyword, Match, Region, Verbatim),
  )
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinGetNumber)
import Ribosome.Nvim.Api.RpcCall (RpcCall(RpcCall))

joinEquals :: Map String String -> String
joinEquals =
  unwords . (equals <$$> Map.toList)
  where
    equals (a, b) = a ++ "=" ++ b

rpcCommand :: [String] -> RpcCall
rpcCommand cmd =
  RpcCall (F "nvim_command") [toMsgpack $ unwords cmd]

synPattern :: String -> String
synPattern text =
  "/" ++ text ++ "/"

namedPattern :: String -> String -> String
namedPattern param text =
  param ++ "=" ++ synPattern text

syntaxItemDetailCmd :: SyntaxItemDetail -> [String]
syntaxItemDetailCmd (Keyword group keyword keywords) =
  ["syntax", "keyword", group, keyword, unwords keywords]
syntaxItemDetailCmd (Match group pat) =
  ["syntax", "match", group, synPattern pat]
syntaxItemDetailCmd (Region group start end skip) =
  ["syntax", "region", group, namedPattern "start" start] ++ foldMap skipArg skip ++ [namedPattern "end" end]
  where
    skipArg a = [namedPattern "skip" a]
syntaxItemDetailCmd (Verbatim cmd) =
  [cmd]

syntaxItemCmd :: SyntaxItem -> [String]
syntaxItemCmd (SyntaxItem detail options params) =
  syntaxItemDetailCmd detail ++ [unwords options, joinEquals params]

highlightCmd :: Highlight -> [String]
highlightCmd (Highlight group values) =
  ["highlight", group, joinEquals values]

hilinkCmd :: HiLink -> [String]
hilinkCmd (HiLink group target) =
  ["highlight", "link", group, target]

syntaxCmds :: Syntax -> [[String]]
syntaxCmds (Syntax items highlights hilinks) =
  (syntaxItemCmd <$> items) ++ (highlightCmd <$> highlights) ++ (hilinkCmd <$> hilinks)

executeSyntax :: NvimE e m => Syntax -> m [Object]
executeSyntax =
  atomic . (rpcCommand <$$> syntaxCmds)

executeWindowSyntax :: NvimE e m => Window -> Syntax -> m [Object]
executeWindowSyntax win syntax = do
  number <- nvimWinGetNumber win
  atomic $ wrapCmd (show number ++ "windo") <$> syntaxCmds syntax
  where
    wrapCmd wrap cmd =
      rpcCommand (wrap : cmd)
