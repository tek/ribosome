module Ribosome.Menu.Prompt.Data.PromptRenderer where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptRenderer :: EffectRow -> Type where
  PromptRenderer :: {
    acquire :: Sem r a,
    release :: a -> Sem r (),
    render :: Prompt -> Sem r ()
  } -> PromptRenderer r

hoistPromptRenderer ::
  (∀ x . Sem r x -> Sem r' x) ->
  PromptRenderer r ->
  PromptRenderer r'
hoistPromptRenderer f PromptRenderer {..} =
  PromptRenderer {
    acquire = f acquire,
    release = f . release,
    render = f . render
  }
