module Ribosome.Api.Autocmd where

import Control.Exception.Lifted (bracket)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (bufferGetNumber, vimCommand, vimGetOption, vimSetOption)

doautocmd ::
  NvimE e m =>
  Bool ->
  Text ->
  m ()
doautocmd silent name =
  vimCommand $ pre <> "doautocmd " <> name
  where
    pre =
      if silent then "silent! " else ""

uautocmd ::
  NvimE e m =>
  Bool ->
  Text ->
  m ()
uautocmd silent name =
  doautocmd silent $ "User " <> name

eventignore ::
  NvimE e m =>
  MonadBaseControl IO m =>
  m a ->
  m a
eventignore =
  bracket getAndSet restore . const
  where
    getAndSet = do
      previous <- vimGetOption "eventignore"
      vimSetOption "eventignore" (toMsgpack ("all" :: Text))
      return previous
    restore =
      vimSetOption "eventignore"

bufferAutocmd ::
  NvimE e m =>
  Buffer ->
  Text ->
  Text ->
  Text ->
  m ()
bufferAutocmd buffer grp event cmd = do
  number <- bufferGetNumber buffer
  vimCommand $ "augroup " <> grp
  vimCommand $ "autocmd " <> " " <> event <> " <buffer=" <> show number <> "> " <> cmd
  vimCommand "augroup end"
