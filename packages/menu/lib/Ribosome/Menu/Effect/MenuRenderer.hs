module Ribosome.Menu.Effect.MenuRenderer where

import Conc (PScoped, pscoped)

import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Menu.Data.Menu (Menu)

data MenuRenderer item :: Effect where
  MenuRender :: Menu item -> MenuRenderer item m ()

makeSem ''MenuRenderer

withMenuRenderer ::
  Member (PScoped par res (MenuRenderer item)) r =>
  par ->
  InterpreterFor (MenuRenderer item) r
withMenuRenderer =
  pscoped

type NvimRenderer item =
  Scoped ScratchId (MenuRenderer item)
