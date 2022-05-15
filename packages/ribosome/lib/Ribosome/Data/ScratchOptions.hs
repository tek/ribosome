module Ribosome.Data.ScratchOptions where

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
    modify :: Bool,
    float :: Maybe FloatOptions,
    size :: Maybe Int,
    maxSize :: Maybe Int,
    syntax :: [Syntax],
    mappings :: [Mapping],
    filetype :: Maybe Text,
    name :: Text
  }
  deriving stock (Eq, Show)

defaultScratchOptions :: Text -> ScratchOptions
defaultScratchOptions = ScratchOptions False False False False True True False Nothing Nothing Nothing [] [] Nothing

instance Default ScratchOptions where
  def =
    defaultScratchOptions "scratch"
