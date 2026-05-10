module Ribosome.Menu.Effect.MenuStream where

import Streamly.Data.Stream (Stream)

data MenuStream :: Effect where
  MenuStream ::
    Stream IO i ->
    m (Maybe query) ->
    (query -> m render) ->
    ([i] -> m render) ->
    (render -> m ()) ->
    m () ->
    MenuStream m ()

makeSem ''MenuStream
