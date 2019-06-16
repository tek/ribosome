{-# LANGUAGE DeriveAnyClass #-}

module Ribosome.Menu.Data.Menu where

import Data.DeepLenses (DeepLenses(..))

import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuFilter =
  MenuFilter Text
  deriving (Eq, Show)

instance Default MenuFilter where
  def = MenuFilter ""

data Menu a =
  Menu {
    _items :: [MenuItem a],
    _filtered :: [MenuItem a],
    _stack :: [(Text, [MenuItem a])],
    _selected :: Int,
    _currentFilter :: MenuFilter
  }
  deriving (Eq, Show, Generic, Default)

makeClassy ''Menu

instance DeepLenses (Menu a) (Menu a) where
  deepLens = id
