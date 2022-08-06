module Ribosome.Menu.Scratch where

import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import Ribosome.Lens ((<|>~))

menuScratch :: ScratchOptions
menuScratch =
  scratch "ribosome-menu"

menuScratchSized :: Int -> ScratchOptions
menuScratchSized n =
  menuScratch & #size ?~ n

ensureSize :: Int -> ScratchOptions -> ScratchOptions
ensureSize n =
  #size <|>~ Just n
