module Ribosome.Data.ScratchOptions(
  ScratchOptions(..),
  defaultScratchOptions,
) where

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
