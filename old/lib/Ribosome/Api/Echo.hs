module Ribosome.Api.Echo where

import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Host.Api.Effect (vimCommand, nvimEcho)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)

echoWith :: Member Rpc r => Text -> Text -> m ()
echoWith cmd msg =
  vimCommand [exon|#{cmd} '#{escapeQuotes msg}'|]

prefixedEchoWith cmd msg = do
  name <- pluginName
  echoWith cmd [exon|#{name}: #{msg}|]

echohl :: Member Rpc r => Bool -> Text -> Text -> m ()
echohl history hl msg =
  nvimEcho [toMsgpack @[Text] [msg, hl]] history mempty

echoHist :: Member Rpc r => Bool -> Text -> m ()
echoHist history msg = do
  nvimEcho [toMsgpack msg] history mempty

prefixedEcho history msg = do
  name <- pluginName
  echoHist history [exon|#{name}: #{msg}|]

echo' :: Member Rpc r => Text -> m ()
echo' =
  echoHist False

echo =
  prefixedEcho False

echom' :: Member Rpc r => Text -> m ()
echom' =
  echoHist True

echom =
  prefixedEcho True

echomS ::
  Member Rpc r =>
  Show a =>
  a ->
  m ()
echomS = echom . show

echon :: Member Rpc r => Text -> m ()
echon =
  echoWith "echon"
