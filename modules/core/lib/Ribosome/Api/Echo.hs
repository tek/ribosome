module Ribosome.Api.Echo where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Data.String (escapeQuotes)
import Ribosome.Nvim.Api.IO (vimCommand)

echom' :: NvimE e m => String -> m ()
echom' msg =
  vimCommand $ "echom '" ++ concatMap escapeQuotes msg ++ "'"

echom :: MonadRibo m => NvimE e m => String -> m ()
echom msg = do
  name <- pluginName
  echom' $ name ++ ": " ++ msg

echomS ::
  MonadRibo m =>
  NvimE e m =>
  Show a =>
  a ->
  m ()
echomS = echom . show
