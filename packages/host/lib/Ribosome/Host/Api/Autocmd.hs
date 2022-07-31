module Ribosome.Host.Api.Autocmd where

import Data.MessagePack (Object)
import Prelude hiding (group)

import Ribosome.Host.Api.Data (nvimCreateAugroup, nvimCreateAutocmd)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcType (
  AutocmdEvents (AutocmdEvents),
  AutocmdGroup (AutocmdGroup),
  AutocmdId (AutocmdId),
  AutocmdOptions (..),
  AutocmdPatterns (AutocmdPatterns),
  )

withAugroup :: Maybe AutocmdGroup -> (Map Text Object -> RpcCall a) -> RpcCall a
withAugroup (Just (AutocmdGroup g)) f =
  nvimCreateAugroup g [("clear", toMsgpack False)] *> f [("group", toMsgpack g)]
withAugroup Nothing f =
  f mempty

autocmd :: AutocmdEvents -> AutocmdOptions -> Text -> RpcCall AutocmdId
autocmd (AutocmdEvents events) AutocmdOptions {pat = AutocmdPatterns pat, ..} cmd =
  withAugroup group \ grp -> AutocmdId <$> nvimCreateAutocmd events (opts <> grp)
  where
    opts =
      msgpackMap ("pattern", pat) ("command", cmd) ("once", once) ("nested", nested)
