-- |Scratch buffer configuration.
module Ribosome.Data.ScratchOptions where

import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Mapping (Mapping)
import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.Syntax (Syntax)

-- |Configure the visual properties of a scratch buffer.
-- If the option @float@ is specified, the buffer will be opened in a floating window.
data ScratchOptions =
  ScratchOptions {
    -- |Whether to open the buffer in a new tab.
    tab :: Bool,
    -- |Whether to split the current window vertically, only relevant for non-floating windows.
    vertical :: Bool,
    -- |Whether to set the @wrap@ option in the window, to disable breaking long lines.
    wrap :: Bool,
    -- |Whether to move the cursor to the window after opening it.
    focus :: Bool,
    -- |Whether to adapt the buffer's size to the number of lines, for horizontal splits.
    resize :: Bool,
    -- |Whether to place the window at the bottom of the stack, only relevant for non-floating windows.
    bottom :: Bool,
    -- |Whether to set the @modifiable@ option for the buffer.
    modify :: Bool,
    -- |If 'Just', creates a floating window with the given config.
    float :: Maybe FloatOptions,
    -- |The initial size of the window.
    size :: Maybe Int,
    -- |When resizing automatically, do not exceed this size.
    maxSize :: Maybe Int,
    -- |A set of syntax rules to apply to the buffer.
    syntax :: [Syntax],
    -- |A set of key mappings to define buffer-locally. See [Ribosome.Mappings]("Ribosome#g:mappings").
    mappings :: [Mapping],
    -- |The value for the @filetype@ option.
    filetype :: Maybe Text,
    -- |The ID of the scratch buffer.
    name :: ScratchId
  }
  deriving stock (Eq, Show, Generic)

-- |The default configuration, setting all flags to 'False' except for 'resize' and 'bottom', and everything else to
-- 'mempty'.
scratch :: ScratchId -> ScratchOptions
scratch name =
  ScratchOptions {
      tab = False,
      vertical = False,
      wrap = False,
      focus = False,
      resize = True,
      bottom = True,
      modify = False,
      float = Nothing,
      size = Nothing,
      maxSize = Nothing,
      syntax = [],
      mappings = [],
      filetype = Nothing,
      ..
    }

instance Default ScratchOptions where
  def =
    scratch "scratch"
