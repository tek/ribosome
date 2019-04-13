module Ribosome.Data.ScratchOptions where

import Data.Default (Default(def))

import Ribosome.Data.Mapping (Mapping)
import Ribosome.Data.Syntax (Syntax)

data ScratchOptions =
  ScratchOptions {
    tab :: Bool,
    vertical :: Bool,
    wrap :: Bool,
    focus :: Bool,
    size :: Maybe Int,
    syntax :: [Syntax],
    mappings :: [Mapping],
    name :: String
  }

defaultScratchOptions :: String -> ScratchOptions
defaultScratchOptions = ScratchOptions False False False False Nothing [] []

instance Default ScratchOptions where
  def = defaultScratchOptions "scratch"

scratchFocus :: ScratchOptions -> ScratchOptions
scratchFocus so =
  so { focus = True }

scratchSyntax :: [Syntax] -> ScratchOptions -> ScratchOptions
scratchSyntax syn so =
  so { syntax = syn }

scratchMappings :: [Mapping] -> ScratchOptions -> ScratchOptions
scratchMappings maps so =
  so { mappings = maps }
