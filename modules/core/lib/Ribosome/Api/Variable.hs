module Ribosome.Api.Variable where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimSetVar)

setVar ::
  NvimE e m =>
  MsgpackEncode a =>
  Text ->
  a ->
  m ()
setVar name =
  vimSetVar name . toMsgpack

setPVar ::
  MonadRibo m =>
  NvimE e m =>
  MsgpackEncode a =>
  Text ->
  a ->
  m ()
setPVar name a = do
  pn <- pluginName
  setVar (pn <> "_" <> name) a
