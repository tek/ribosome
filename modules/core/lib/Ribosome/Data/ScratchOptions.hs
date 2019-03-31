module Ribosome.Data.ScratchOptions(
  ScratchOptions(..),
  defaultScratchOptions,
) where

import Data.Default (Default(def))

data ScratchOptions =
  ScratchOptions {
    tab :: Bool,
    vertical :: Bool,
    size :: Maybe Int,
    wrap :: Bool,
    name :: String
  }

defaultScratchOptions :: String -> ScratchOptions
defaultScratchOptions = ScratchOptions False False Nothing False

instance Default ScratchOptions where
  def = defaultScratchOptions "scratch"
