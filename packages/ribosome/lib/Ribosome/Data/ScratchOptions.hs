module Ribosome.Data.ScratchOptions where

import Control.Lens (set)
import Prelude hiding (modify)

import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Mapping (Mapping)
import Ribosome.Data.Syntax (Syntax)

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
    _name :: Text
  }
  deriving (Eq, Show)

makeClassy ''ScratchOptions

defaultScratchOptions :: Text -> ScratchOptions
defaultScratchOptions = ScratchOptions False False False False True True False Nothing Nothing Nothing [] [] Nothing

instance Default ScratchOptions where
  def = defaultScratchOptions "scratch"

scratchFocus :: ScratchOptions -> ScratchOptions
scratchFocus =
  set focus True

scratchSyntax :: [Syntax] -> ScratchOptions -> ScratchOptions
scratchSyntax =
  set syntax

scratchMappings :: [Mapping] -> ScratchOptions -> ScratchOptions
scratchMappings =
  set mappings

scratchFloat :: FloatOptions -> ScratchOptions -> ScratchOptions
scratchFloat =
  set float . Just

scratchSize :: Int -> ScratchOptions -> ScratchOptions
scratchSize =
  set size . Just

scratchModify :: ScratchOptions -> ScratchOptions
scratchModify =
  set modify True
