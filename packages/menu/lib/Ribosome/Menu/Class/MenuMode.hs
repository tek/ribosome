module Ribosome.Menu.Class.MenuMode where

class (
    Show mode,
    Ord mode
  ) => MenuMode (i :: Type) (mode :: Type) where

    type Filter mode :: Type -> Type

    cycleFilter :: mode -> mode

    renderFilter :: mode -> Text

    renderExtra :: mode -> Maybe Text

    filterMode :: mode -> Filter mode i

instance MenuMode i () where

  type Filter () =
    Const ()

  cycleFilter =
    id

  renderFilter _ =
    "no mode"

  renderExtra _ =
    Nothing

  filterMode () =
    Const () 
