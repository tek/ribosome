module Ribosome.Api.Echo where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Nvim.Api.IO (vimCommand, nvimEcho)
import Ribosome.Msgpack.Encode (toMsgpack)

echoWith :: NvimE e m => Text -> Text -> m ()
echoWith cmd msg =
  vimCommand [exon|#{cmd} '#{escapeQuotes msg}'|]

prefixedEchoWith :: MonadRibo m => NvimE e m => Text -> Text -> m ()
prefixedEchoWith cmd msg = do
  name <- pluginName
  echoWith cmd [exon|#{name}: #{msg}|]

echohl :: NvimE e m => Bool -> Text -> Text -> m ()
echohl history hl msg =
  nvimEcho [toMsgpack @[Text] [msg, hl]] history mempty

echoHist :: NvimE e m => Bool -> Text -> m ()
echoHist history msg = do
  nvimEcho [toMsgpack msg] history mempty

prefixedEcho :: MonadRibo m => NvimE e m => Bool -> Text -> m ()
prefixedEcho history msg = do
  name <- pluginName
  echoHist history [exon|#{name}: #{msg}|]

echo' :: NvimE e m => Text -> m ()
echo' =
  echoHist False

echo :: MonadRibo m => NvimE e m => Text -> m ()
echo =
  prefixedEcho False

echom' :: NvimE e m => Text -> m ()
echom' =
  echoHist True

echom :: MonadRibo m => NvimE e m => Text -> m ()
echom =
  prefixedEcho True

echomS ::
  MonadRibo m =>
  NvimE e m =>
  Show a =>
  a ->
  m ()
echomS = echom . show

echon :: NvimE e m => Text -> m ()
echon =
  echoWith "echon"
