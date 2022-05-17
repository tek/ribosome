module Ribosome.Menu.Data.CursorLine where

newtype CursorLine =
  CursorLine { unCursorLine :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)
