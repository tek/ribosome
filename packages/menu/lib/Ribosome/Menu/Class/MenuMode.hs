module Ribosome.Menu.Class.MenuMode where

class (
    Show mode,
    Ord mode
  ) => MenuMode (i :: Type) (mode :: Type) where

    type Filter mode :: Type -> Type

    cycleFilter :: mode -> mode
    cycleFilter = id

    renderFilter :: mode -> Text
    renderFilter _ = "no mode"

    renderExtra :: mode -> Int -> Maybe Text
    renderExtra _ _ = Nothing

    filterMode :: mode -> Filter mode i

instance MenuMode i () where
  type Filter () = Const ()
  filterMode () = Const ()
