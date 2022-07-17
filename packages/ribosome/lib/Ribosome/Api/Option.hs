module Ribosome.Api.Option where

import Data.MessagePack (Object)
import Data.Text (splitOn)
import Exon (exon)

import Ribosome.Host.Api.Effect (vimGetOption, vimSetOption)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Effect.Rpc (Rpc)

optionCat ::
  Member Rpc r =>
  Text ->
  Text ->
  Sem r ()
optionCat name extra = do
  current <- vimGetOption name
  vimSetOption name [exon|#{current},#{extra}|]

rtpCat ::
  Member Rpc r =>
  Text ->
  Sem r ()
rtpCat =
  optionCat "runtimepath"

optionList ::
  Member Rpc r =>
  Text ->
  Sem r [Text]
optionList name = do
  s <- vimGetOption name
  pure (splitOn "," s)

withOption ::
  ∀ a r b .
  Members [Rpc, Resource] r =>
  MsgpackEncode a =>
  Text ->
  a ->
  Sem r b ->
  Sem r b
withOption name value =
  bracket setOpt reset . const
  where
    setOpt =
      vimGetOption @Object name <* vimSetOption name value
    reset =
      vimSetOption name
