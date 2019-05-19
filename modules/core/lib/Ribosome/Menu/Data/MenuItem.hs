{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Data.MenuItem where

import Control.Lens (makeClassy)

data MenuItem =
  MenuItem {
    _ident :: Text,
    _text :: Text
  }
  deriving (Eq, Show)

makeClassy ''MenuItem
