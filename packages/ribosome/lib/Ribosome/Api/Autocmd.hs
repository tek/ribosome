module Ribosome.Api.Autocmd where

import Exon (exon)

import Ribosome.Host.Api.Data (Buffer, nvimCommand)
import Ribosome.Host.Api.Effect (bufferGetNumber, vimGetOption, vimSetOption)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

doautocmd ::
  Member Rpc r =>
  Text ->
  Sem r ()
doautocmd name =
  Rpc.notify (nvimCommand [exon|doautocmd #{name}|])

uautocmd ::
  Member Rpc r =>
  Text ->
  Sem r ()
uautocmd name =
  doautocmd [exon|User #{name}|]

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
  Text ->
  Text ->
  Text ->
  Sem r ()
bufferAutocmd buffer grp event cmd = do
  number <- bufferGetNumber buffer
  Rpc.sync do
    nvimCommand [exon|augroup #{grp}|]
    nvimCommand [exon|autocmd #{event} <buffer=#{show number}> #{cmd}|]
    nvimCommand "augroup end"
    pure ()
