module Ribosome.Api.Syntax where

import Control.Monad ((<=<))
import Data.Functor.Syntax ((<$$>))
import Data.Map (Map)
import qualified Data.Map as Map (toList)
import Data.MessagePack (Object)
import Neovim.Plugin.Classes (FunctionName(F))

import Ribosome.Api.Atomic (atomic)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Syntax (Highlight(Highlight), Syntax(Syntax), SyntaxItem(Keyword, Match, Region))
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.RpcCall (RpcCall(RpcCall))

joinEquals :: Map String String -> String
joinEquals =
  unwords . (equals <$$> Map.toList)
  where
    equals (a, b) = a ++ "=" ++ b

rpcCommand :: [String] -> RpcCall
rpcCommand cmd =
  RpcCall (F "nvim_command") [toMsgpack $ unwords cmd]

syntaxItemCall :: SyntaxItem -> RpcCall
syntaxItemCall (Keyword group keyword options keywords params) =
  rpcCommand ["syntax", "keyword", group, keyword, unwords keywords, unwords options, joinEquals params]
syntaxItemCall (Match group pat options params) =
  rpcCommand ["syntax", "match", group, "/" ++ pat ++ "/", unwords options, joinEquals params]

highlightCall :: Highlight -> RpcCall
highlightCall (Highlight group values) =
  rpcCommand ["highlight", group, joinEquals values]

executeSyntax :: NvimE e m => Syntax -> m [Object]
executeSyntax (Syntax items highlights hilinks) =
  atomic ((syntaxItemCall <$> items) ++ (highlightCall <$> highlights))
