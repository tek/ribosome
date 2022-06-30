module Ribosome.Data.ScratchOptions where

import Prelude hiding (modify)

import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Mapping (Mapping)
import Ribosome.Data.ScratchId (ScratchId)
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
    name :: ScratchId
  }
  deriving stock (Eq, Show, Generic)

scratch :: ScratchId -> ScratchOptions
scratch =
  ScratchOptions False False False False True True False Nothing Nothing Nothing [] [] Nothing

instance Default ScratchOptions where
  def =
    scratch "scratch"
