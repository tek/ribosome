{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Data.Menu where

import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuFilter =
  MenuFilter Text
  deriving (Eq, Show)

instance Default MenuFilter where
  def = MenuFilter ""

data Menu =
  Menu {
    _items :: [MenuItem],
    _filtered :: [MenuItem],
    _stack :: [(Text, [MenuItem])],
    _selected :: Int,
    _currentFilter :: MenuFilter
  }
  deriving (Eq, Show, Generic, Default)

deepLenses ''Menu
