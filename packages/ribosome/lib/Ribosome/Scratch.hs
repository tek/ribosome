-- A scratch buffer is what Neovim calls text not associated with a file, used for informational or interactive content.
-- Ribosome provides an interface for maintaining those, by associating a view configuration with an ID and allowing to
-- update the text displayed in it.
module Ribosome.Scratch (
  module Ribosome.Effect.Scratch,
  module Ribosome.Data.ScratchOptions,
  module Ribosome.Data.ScratchId,
  module Ribosome.Data.ScratchState,
  module Ribosome.Data.FloatOptions,
  module Ribosome.Interpreter.Scratch,
) where

import Ribosome.Data.FloatOptions
import Ribosome.Data.ScratchId
import Ribosome.Data.ScratchOptions
import Ribosome.Data.ScratchState
import Ribosome.Effect.Scratch
import Ribosome.Interpreter.Scratch
