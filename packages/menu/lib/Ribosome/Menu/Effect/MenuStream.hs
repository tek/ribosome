module Ribosome.Menu.Effect.MenuStream where

import Streamly.Prelude (SerialT)

data MenuStream :: Effect where
  MenuStream ::
    SerialT IO i ->
    m (Maybe query) ->
    (query -> m render) ->
    ([i] -> m render) ->
    (render -> m ()) ->
    m () ->
    MenuStream m ()

makeSem ''MenuStream
