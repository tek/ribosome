module Ribosome.Data.ScratchOptions where

import Data.Default (Default(def))

import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Mapping (Mapping)
import Ribosome.Data.Syntax (Syntax)

data ScratchOptions =
  ScratchOptions {
    tab :: Bool,
    vertical :: Bool,
    wrap :: Bool,
    focus :: Bool,
    resize :: Bool,
    bottom :: Bool,
    float :: Maybe FloatOptions,
    size :: Maybe Int,
    maxSize :: Maybe Int,
    syntax :: [Syntax],
    mappings :: [Mapping],
    name :: Text
  }

defaultScratchOptions :: Text -> ScratchOptions
defaultScratchOptions = ScratchOptions False False False False True True Nothing Nothing Nothing [] []

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
