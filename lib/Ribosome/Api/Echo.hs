module Ribosome.Api.Echo(
  echom,
  echomS,
  escapeQuotes,
) where

import Control.Monad.Error.Class (MonadError)

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, pluginName)
import Ribosome.Data.String (escapeQuotes)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Nvim.Api.RpcCall (RpcError)

echom :: (MonadError RpcError m, MonadRibo m, Nvim m) => String -> m ()
echom msg = do
  name <- pluginName
  vimCommand $ "echom '" ++ name ++ ": " ++ concatMap escapeQuotes msg ++ "'"

echomS :: (MonadError RpcError m, MonadRibo m, Nvim m, Show a) => a -> m ()
echomS = echom . show
