module Ribosome.Api.Buffer where

import Control.Monad (when)
import Data.MessagePack (Object)

import Ribosome.Api.Atomic (atomicAs)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, bufferGetName)
import Ribosome.Nvim.Api.IO (
  bufferGetLines,
  bufferGetNumber,
  bufferIsValid,
  bufferSetLines,
  vimCallFunction,
  vimCommand,
  vimGetBuffers,
  vimGetCurrentBuffer,
  )
import Ribosome.Nvim.Api.RpcCall (syncRpcCall)

edit :: NvimE e m => FilePath -> m ()
edit path = vimCommand $ "silent! edit " <> toText path

nvimCallBool :: NvimE e m => Text -> [Object] -> m Bool
nvimCallBool =
  vimCallFunction

buflisted :: NvimE e m => Buffer -> m Bool
buflisted buf = do
  num <- bufferGetNumber buf
  nvimCallBool "buflisted" [toMsgpack num]

bufferContent :: NvimE e m => Buffer -> m [Text]
bufferContent buffer =
  bufferGetLines buffer 0 (-1) False

currentBufferContent :: NvimE e m => m [Text]
currentBufferContent =
  bufferContent =<< vimGetCurrentBuffer

setBufferContent :: NvimE e m => Buffer -> [Text] -> m ()
setBufferContent buffer =
  bufferSetLines buffer 0 (-1) False

setCurrentBufferContent ::
  NvimE e m =>
  [Text] ->
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
    vimCommand ("silent! bdelete! " <> show number)

wipeBuffer ::
  NvimE e m =>
  Buffer ->
  m ()
wipeBuffer buffer = do
  valid <- bufferIsValid buffer
  when valid $ do
    number <- bufferGetNumber buffer
    vimCommand ("silent! bwipeout! " <> show number)

buffersAndNames ::
  MonadIO m =>
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  m [(Buffer, Text)]
buffersAndNames = do
  buffers <- vimGetBuffers
  names <- atomicAs (syncRpcCall . bufferGetName <$> buffers)
  return (zip buffers names)

bufferForFile ::
  MonadIO m =>
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  Text ->
  m (Maybe Buffer)
bufferForFile target =
  fmap fst . find sameBuffer <$> buffersAndNames
  where
    sameBuffer (_, name) = name == target
