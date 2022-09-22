-- |API functions for tags.
module Ribosome.Api.Tags where

import Path (Abs, File, Path)

import Ribosome.Data.Tag (Tag)
import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Path (pathText)

-- |Return all tags matching the supplied regex (default @.@), optionally prioritizing the supplied file.
taglist ::
  Member Rpc r =>
  Maybe Text ->
  Maybe (Path Abs File) ->
  Sem r [Tag]
taglist expr file =
  nvimCallFunction "taglist" (toMsgpack <$> (fromMaybe "." expr : foldMap (pure . pathText) file))
