module Ribosome.Api.Echo where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Nvim.Api.IO (vimCommand)

echoWith :: NvimE e m => Text -> Text -> m ()
echoWith cmd msg =
  vimCommand $ cmd <> " '" <> escapeQuotes msg <> "'"

echoWithName :: MonadRibo m => NvimE e m => Text -> Text -> m ()
echoWithName cmd msg = do
  name <- pluginName
  echoWith cmd $ name <> ": " <> msg

echo' :: NvimE e m => Text -> m ()
echo' =
  echoWith "echo"

echo :: MonadRibo m => NvimE e m => Text -> m ()
echo =
  echoWithName "echo"

echom' :: NvimE e m => Text -> m ()
echom' =
  echoWith "echom"

echom :: MonadRibo m => NvimE e m => Text -> m ()
echom =
  echoWithName "echom"

echomS ::
  MonadRibo m =>
  NvimE e m =>
  Show a =>
  a ->
  m ()
echomS = echom . show

echon :: MonadRibo m => NvimE e m => Text -> m ()
echon =
  echoWithName "echom"

echohl :: NvimE e m => Text -> m ()
echohl =
  vimCommand . ("echohl " <>)
