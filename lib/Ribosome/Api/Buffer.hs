module Ribosome.Api.Buffer  where

import Data.MessagePack (Object)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (
  bufferGetLines,
  bufferGetNumber,
  bufferSetLines,
  vimCallFunction,
  vimCommand,
  vimGetCurrentBuffer,
  )

edit :: NvimE e m => FilePath -> m ()
edit path = vimCommand $ "silent! edit " ++ path

nvimCallBool :: NvimE e m => String -> [Object] -> m Bool
nvimCallBool =
  vimCallFunction

buflisted :: NvimE e m => Buffer -> m Bool
buflisted buf = do
  num <- bufferGetNumber buf
  nvimCallBool "buflisted" [toMsgpack num]

bufferContent :: NvimE e m => Buffer -> m [String]
bufferContent buffer =
  bufferGetLines buffer 0 (-1) False

currentBufferContent :: NvimE e m => m [String]
currentBufferContent =
  bufferContent =<< vimGetCurrentBuffer

setBufferContent :: NvimE e m => Buffer -> [String] -> m ()
setBufferContent buffer =
  bufferSetLines buffer 0 (-1) False

setCurrentBufferContent :: NvimE e m => [String] -> m ()
setCurrentBufferContent content = do
  buffer <- vimGetCurrentBuffer
  setBufferContent buffer content
