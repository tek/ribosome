module Ribosome.Menu.Scratch where

import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import Ribosome.Lens ((<|>~))
import Ribosome.Data.ScratchId (ScratchId)

menuItemsScratchId :: ScratchId
menuItemsScratchId = "ribosome-menu-items"

menuScratch :: ScratchOptions
menuScratch = scratch menuItemsScratchId

menuScratchSized :: Int -> ScratchOptions
menuScratchSized n =
  menuScratch & #size ?~ n

ensureSize :: Int -> ScratchOptions -> ScratchOptions
ensureSize n =
  #size <|>~ Just n
