module Ribosome.Api.Option where

import Control.Exception.Lifted (bracket)
import Data.Text (splitOn)

import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Host.Api.Effect (vimGetOption, vimSetOption)

optionCat :: Member Rpc r => Text -> Text -> m ()
optionCat name extra = do
  current <- vimGetOption name
  vimSetOption name $ toMsgpack $ current <> "," <> extra

rtpCat :: Member Rpc r => Text -> m ()
rtpCat = optionCat "runtimepath"

optionString :: Member Rpc r => Text -> m Text
optionString = vimGetOption

optionList :: Member Rpc r => Text -> m [Text]
optionList name = do
  s <- vimGetOption name
  pure $ splitOn "," s

withOption ::
  Member Rpc r =>
  MsgpackEncode a =>
  Text ->
  a ->
  m b ->
  m b
withOption name value =
  bracket set reset . const
  where
    set =
      vimGetOption name <* vimSetOption name (toMsgpack value)
    reset =
      vimSetOption name
