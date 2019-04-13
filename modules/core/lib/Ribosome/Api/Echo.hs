module Ribosome.Api.Echo where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Data.String (escapeQuotes)
import Ribosome.Nvim.Api.IO (vimCommand)

echoWith :: NvimE e m => String -> String -> m ()
echoWith cmd msg =
  vimCommand $ cmd ++ " '" ++ concatMap escapeQuotes msg ++ "'"

echoWithName :: MonadRibo m => NvimE e m => String -> String -> m ()
echoWithName cmd msg = do
  name <- pluginName
  echoWith cmd $ name ++ ": " ++ msg

echo' :: NvimE e m => String -> m ()
echo' =
  echoWith "echo"

echo :: MonadRibo m => NvimE e m => String -> m ()
echo =
  echoWithName "echo"

echom' :: NvimE e m => String -> m ()
echom' =
  echoWith "echom"

echom :: MonadRibo m => NvimE e m => String -> m ()
echom =
  echoWithName "echom"

echomS ::
  MonadRibo m =>
  NvimE e m =>
  Show a =>
  a ->
  m ()
echomS = echom . show
