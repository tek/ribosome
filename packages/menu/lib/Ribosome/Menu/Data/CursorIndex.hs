module Ribosome.Menu.Data.CursorIndex where

newtype CursorIndex =
  CursorIndex { unCursorIndex :: Word }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance Default CursorIndex where
  def = 0
