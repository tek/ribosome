module Ribosome.Menu.Effect.PromptStream where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)

data PromptStream :: Effect where
  PromptStream ::
    m (Maybe PromptControlEvent) ->
    (Prompt -> m ()) ->
    (Either PromptControlEvent PromptInputEvent -> m (Maybe (Prompt, PromptEvent))) ->
    m () ->
    m () ->
    m InputEvent ->
    PromptStream m (SerialT IO (Prompt, PromptEvent))

makeSem ''PromptStream
