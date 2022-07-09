module Ribosome.Menu.Effect.MenuRenderer where

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Menu.Data.Menu (Menu)

data MenuRenderer item :: Effect where
  MenuRender :: Menu item -> MenuRenderer item m ()

makeSem ''MenuRenderer

withMenuRenderer ::
  Member (Scoped res (MenuRenderer item)) r =>
  InterpreterFor (MenuRenderer item) r
withMenuRenderer =
  scoped

type NvimRenderer item =
  Scoped ScratchId (MenuRenderer item)
