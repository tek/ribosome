module Ribosome.Api.Buffer where

import Control.Monad (when)
import Data.Bifunctor (second)
import Data.MessagePack (Object)
import qualified Data.Text as Text (null)
import System.FilePath ((</>))

import Ribosome.Api.Atomic (atomicAs)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer)
import qualified Ribosome.Nvim.Api.Data as ApiData (bufferGetName)
import Ribosome.Nvim.Api.IO (
  bufferGetLines,
  bufferGetName,
  bufferGetNumber,
  bufferGetOption,
  bufferIsValid,
  bufferSetLines,
  nvimWinSetBuf,
  vimCallFunction,
  vimCommand,
  vimGetBuffers,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
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
  names <- atomicAs (syncRpcCall . ApiData.bufferGetName <$> buffers)
  return (zip buffers names)

bufferForFile ::
  MonadIO m =>
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  Text ->
  m (Maybe Buffer)
bufferForFile target = do
  cwd <- nvimCwd
  fmap fst . find sameBuffer . fmap (second (toText . absolute cwd . toString)) <$> buffersAndNames
  where
    sameBuffer (_, name) = name == target
    absolute dir ('.' : '/' : rest) =
      dir </> rest
    absolute _ p@('/' : _) =
      p
    absolute dir path =
      dir </> path

currentBufferName ::
  NvimE e m =>
  m Text
currentBufferName =
  bufferGetName =<< vimGetCurrentBuffer

setCurrentBuffer ::
  NvimE e m =>
  Buffer ->
  m ()
setCurrentBuffer buf = do
  win <- vimGetCurrentWindow
  nvimWinSetBuf win buf

bufferIsFile ::
  NvimE e m =>
  Buffer ->
  m Bool
bufferIsFile buf =
  Text.null <$> bufferGetOption buf "buftype"

bufferCount ::
  NvimE e m =>
  m Natural
bufferCount =
  fromIntegral . length <$> vimGetBuffers
