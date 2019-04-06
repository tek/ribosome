module Ribosome.Data.ScratchOptions where

import Data.Default (Default(def))

import Ribosome.Data.Syntax (Syntax)

data ScratchOptions =
  ScratchOptions {
    tab :: Bool,
    vertical :: Bool,
    size :: Maybe Int,
    wrap :: Bool,
    syntax :: [Syntax],
    name :: String
  }

defaultScratchOptions :: String -> ScratchOptions
defaultScratchOptions = ScratchOptions False False Nothing False []

instance Default ScratchOptions where
  def = defaultScratchOptions "scratch"

scratchSyntax :: [Syntax] -> ScratchOptions -> ScratchOptions
scratchSyntax syn so =
  so { syntax = syn }
