module Ribosome.Api.Autocmd (
  module Ribosome.Api.Autocmd,
  autocmd,
) where

import Data.MessagePack (Object)

import Ribosome.Host.Api.Autocmd (autocmd)
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Effect (bufferGetNumber, nvimExecAutocmds, vimGetOption, vimSetOption)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcType (AutocmdEvents, AutocmdOptions, buffer, AutocmdId)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

doautocmdWith ::
  Member Rpc r =>
  AutocmdEvents ->
  Map Text Object ->
  Sem r ()
doautocmdWith =
  nvimExecAutocmds

doautocmd ::
  Member Rpc r =>
  AutocmdEvents ->
  Sem r ()
doautocmd events =
  nvimExecAutocmds events mempty

uautocmd ::
  Member Rpc r =>
  Text ->
  Sem r ()
uautocmd name =
  doautocmdWith "User" [("pattern", toMsgpack name)]

eventignore ::
  Members [Rpc, Resource] r =>
  Sem r a ->
  Sem r a
eventignore =
  bracket getAndSet restore . const
  where
    getAndSet = do
      previous :: Text <- vimGetOption "eventignore"
      vimSetOption "eventignore" ("all" :: Text)
      pure previous
    restore =
      vimSetOption "eventignore"

bufferAutocmd ::
  Member Rpc r =>
  Buffer ->
  AutocmdEvents ->
  AutocmdOptions ->
  Text ->
  Sem r AutocmdId
bufferAutocmd buf events options cmd = do
  number <- bufferGetNumber buf
  Rpc.sync do
    autocmd events options { buffer = Just number } cmd
