{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Data.Menu where

import Data.Default (Default)

import Ribosome.Menu.Data.MenuItem (MenuItem)

data Menu =
  Menu {
    _items :: [MenuItem],
    _filtered :: [MenuItem],
    _stack :: [(Text, [MenuItem])],
    _selected :: Int
  }
  deriving (Eq, Show, Generic, Default)

deepLenses ''Menu
