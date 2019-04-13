module Ribosome.Api.Buffer  where

import Control.Monad (when)
import Data.MessagePack (Object)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (
  bufferGetLines,
  bufferGetNumber,
  bufferIsValid,
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

setCurrentBufferContent ::
  NvimE e m =>
  [String] ->
  m ()
setCurrentBufferContent content = do
  buffer <- vimGetCurrentBuffer
  setBufferContent buffer content

closeBuffer ::
  NvimE e m =>
  Buffer ->
  m ()
closeBuffer buffer = do
  valid <- bufferIsValid buffer
  when valid $ do
    number <- bufferGetNumber buffer
    vimCommand ("silent! bdelete! " ++ show number)

wipeBuffer ::
  NvimE e m =>
  Buffer ->
  m ()
wipeBuffer buffer = do
  valid <- bufferIsValid buffer
  when valid $ do
    number <- bufferGetNumber buffer
    vimCommand ("silent! bwipeout! " ++ show number)
