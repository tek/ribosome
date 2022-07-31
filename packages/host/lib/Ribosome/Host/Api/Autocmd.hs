module Ribosome.Host.Api.Autocmd where

import Data.MessagePack (Object)
import Prelude hiding (group)

import Ribosome.Host.Api.Data (nvimCreateAugroup, nvimCreateAutocmd)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcType (
  AutocmdBuffer (AutocmdBuffer),
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
autocmd (AutocmdEvents events) AutocmdOptions {..} cmd =
  withAugroup group \ grp -> AutocmdId <$> nvimCreateAutocmd events (opts <> bufPat <> grp)
  where
    opts =
      msgpackMap ("command", cmd) ("once", once) ("nested", nested)
    bufPat =
      either  bufOpt patternOpt target
    patternOpt (AutocmdPatterns pat) =
      [("pattern", toMsgpack pat)]
    bufOpt (AutocmdBuffer buf) =
      [("buffer", toMsgpack buf)]
