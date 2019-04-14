module Ribosome.Api.Option where

import Data.Text (splitOn)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimGetOption, vimSetOption)

optionCat :: NvimE e m => Text -> Text -> m ()
optionCat name extra = do
  current <- vimGetOption name
  vimSetOption name $ toMsgpack $ current <> "," <> extra

rtpCat :: NvimE e m => Text -> m ()
rtpCat = optionCat "runtimepath"

optionString :: NvimE e m => Text -> m Text
optionString = vimGetOption

optionList :: NvimE e m => Text -> m [Text]
optionList name = do
  s <- vimGetOption name
  return $ splitOn "," s
