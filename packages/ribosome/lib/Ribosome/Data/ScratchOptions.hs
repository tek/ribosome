module Ribosome.Data.ScratchOptions where

import Control.Lens (makeClassy)
import Prelude hiding (modify)

import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Mapping (Mapping)
import Ribosome.Data.Syntax (Syntax)
import Ribosome.Data.ScratchState (ScratchId)

data ScratchOptions =
  ScratchOptions {
    _tab :: Bool,
    _vertical :: Bool,
    _wrap :: Bool,
    _focus :: Bool,
    _resize :: Bool,
    _bottom :: Bool,
    _modify :: Bool,
    _float :: Maybe FloatOptions,
    _size :: Maybe Int,
    _maxSize :: Maybe Int,
    _syntax :: [Syntax],
    _mappings :: [Mapping],
    _filetype :: Maybe Text,
    _name :: ScratchId
  }
  deriving stock (Eq, Show)

makeClassy ''ScratchOptions

defaultScratchOptions :: ScratchId -> ScratchOptions
defaultScratchOptions =
  ScratchOptions False False False False True True False Nothing Nothing Nothing [] [] Nothing

instance Default ScratchOptions where
  def =
    defaultScratchOptions "scratch"
