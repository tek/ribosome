module Ribosome.Control.StrictRibosome where

import Prelude hiding (state)

import Ribosome.Control.Ribosome (RibosomeState)

data StrictRibosome s =
  StrictRibosome {
    _name :: Text,
    _state :: RibosomeState s
    }

makeClassy ''StrictRibosome

instance Default s => Default (StrictRibosome s) where
  def = StrictRibosome "default" def
