module Ribosome.Api.Option where

import Data.List.Split (splitOn)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimGetOption, vimSetOption)

optionCat :: NvimE e m => String -> String -> m ()
optionCat name extra = do
  current <- vimGetOption name
  vimSetOption name $ toMsgpack $ current ++ "," ++ extra

rtpCat :: NvimE e m => String -> m ()
rtpCat = optionCat "runtimepath"

optionString :: NvimE e m => String -> m String
optionString = vimGetOption

optionList :: NvimE e m => String -> m [String]
optionList name = do
  s <- vimGetOption name
  return $ splitOn "," s
