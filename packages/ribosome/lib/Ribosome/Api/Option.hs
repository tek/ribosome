-- |API functions for Neovim options.
module Ribosome.Api.Option where

import Data.MessagePack (Object)
import Data.Text (splitOn)
import Exon (exon)

import Ribosome.Host.Api.Effect (vimGetOption, vimSetOption, nvimSetOption)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Effect.Rpc (Rpc)
import qualified Data.Text as Text

-- |Append a string to a comma-separated option.
optionCat ::
  Member Rpc r =>
  Text ->
  Text ->
  Sem r ()
optionCat name extra = do
  current <- vimGetOption name
  vimSetOption name [exon|#{current},#{extra}|]

-- |Append a string to the option @runtimepath@.
rtpCat ::
  Member Rpc r =>
  Text ->
  Sem r ()
rtpCat =
  optionCat "runtimepath"

-- |Get a list of strings from a comma-separated option.
optionList ::
  Member Rpc r =>
  Text ->
  Sem r [Text]
optionList name = do
  s <- vimGetOption name
  pure (splitOn "," s)

-- |Set an option to a comma-separated list of strings.
optionSetList ::
  Member Rpc r =>
  Text ->
  [Text] ->
  Sem r ()
optionSetList name values =
  nvimSetOption name (Text.intercalate "," values)

-- |Run an action with an option temporarily set to a value, then restore the old value.
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
