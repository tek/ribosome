-- |The 'Settings' API, an effect for accessing Neovim variables with defaults.
module Ribosome.Settings (
  module Ribosome.Data.Setting,
  module Ribosome.Data.SettingError,
  module Ribosome.Settings,
  module Ribosome.Effect.Settings,
) where

import Ribosome.Data.Setting (Setting (..))
import Ribosome.Data.SettingError (SettingError (..))
import Ribosome.Effect.Settings (Settings, get, maybe, or, update)

-- |The vertical margin for floating windows used by @ribosome-menu@.
menuMarginVertical :: Setting Float
menuMarginVertical =
  Setting "ribosome_menu_margin_vertical" False (Just 0.2)

-- |The horizontal margin for floating windows used by @ribosome-menu@.
menuMarginHorizontal :: Setting Float
menuMarginHorizontal =
  Setting "ribosome_menu_margin_horizontal" False (Just 0.1)
