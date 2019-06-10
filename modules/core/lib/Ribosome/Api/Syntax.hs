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
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinGetNumber, vimGetCurrentWindow, vimSetCurrentWindow)
import Ribosome.Nvim.Api.RpcCall (RpcCall(RpcCall))

joinEquals :: Map Text Text -> Text
joinEquals =
  unwords . (equals <$$> Map.toList)
  where
    equals (a, b) = a <> "=" <> b

rpcCommand :: [Text] -> RpcCall
rpcCommand cmd =
  RpcCall (F "nvim_command") [toMsgpack $ unwords cmd]

synPattern :: Text -> Text
synPattern text =
  "/" <> text <> "/"

namedPattern :: Text -> Text -> Text -> Text
namedPattern param text offset =
  param <> "=" <> synPattern text <> offset

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
  ["highlight", group', joinEquals values]

hilinkCmd :: HiLink -> [Text]
hilinkCmd (HiLink group' target) =
  ["highlight", "link", group', target]

syntaxCmds :: Syntax -> [[Text]]
syntaxCmds (Syntax items highlights hilinks) =
  (syntaxItemCmd <$> items) <> (highlightCmd <$> highlights) <> (hilinkCmd <$> hilinks)

executeSyntax ::
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  Syntax ->
  m [Object]
executeSyntax =
  atomic . (rpcCommand <$$> syntaxCmds)

executeWindowSyntax ::
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  Window ->
  Syntax ->
  m [Object]
executeWindowSyntax win syntax = do
  previous <- vimGetCurrentWindow
  number <- nvimWinGetNumber win
  exec number <* vimSetCurrentWindow previous
  where
    exec number =
      atomic $ wrapCmd (show number <> "windo") <$> syntaxCmds syntax
    wrapCmd wrap cmd =
      rpcCommand (wrap : cmd)
