-- |Autocmd functions.
module Ribosome.Api.Autocmd (
  module Ribosome.Api.Autocmd,
  autocmd,
) where

import Data.MessagePack (Object)

import Ribosome.Host.Api.Autocmd (autocmd)
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Data (bufferGetNumber, nvimExecAutocmds, vimGetOption, vimSetOption)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcType (AutocmdBuffer (AutocmdBuffer), AutocmdEvents, AutocmdId, AutocmdOptions, target)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Trigger a set of autocmds.
--
-- Same as 'nvimExecAutocmds', but specializing the parameter type.
doautocmdWith ::
  Member Rpc r =>
  AutocmdEvents ->
  Map Text Object ->
  Sem r ()
doautocmdWith =
  nvimExecAutocmds

-- |Trigger a set of autocmds.
doautocmd ::
  Member Rpc r =>
  AutocmdEvents ->
  Sem r ()
doautocmd events =
  nvimExecAutocmds events mempty

-- |Trigger a user autocmd.
uautocmd ::
  Member Rpc r =>
  Text ->
  Sem r ()
uautocmd name =
  doautocmdWith "User" [("pattern", toMsgpack name)]

-- |Execute an action with all autocmds disabled.
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

-- |Create an autocmd in a buffer.
bufferAutocmd ::
  Member Rpc r =>
  Buffer ->
  AutocmdEvents ->
  AutocmdOptions ->
  -- |Command to execute when the autocmd triggers.
  Text ->
  Sem r AutocmdId
bufferAutocmd buf events options cmd = do
  number <- bufferGetNumber buf
  Rpc.sync do
    autocmd events options { target = Left (AutocmdBuffer number) } cmd
