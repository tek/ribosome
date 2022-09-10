module Ribosome.Menu.Mappings where

import Ribosome.Data.Mapping (MapMode (MapInsert), MappingSpec)
import Ribosome.Menu.Action (MenuWidget, menuCycle, menuCycleFilter, menuQuit, menuToggle, menuToggleAll)
import Ribosome.Menu.Class.FilterEnum (FilterEnum)

type Mappings f i r a =
  Map MappingSpec (MenuWidget f i r a)

withInsert :: MappingSpec -> MappingSpec
withInsert =
  #mode %~ ([MapInsert] <>)

insert :: MappingSpec -> MappingSpec
insert =
  #mode .~ [MapInsert]

defaultMappings ::
  FilterEnum f =>
  Mappings f i r a
defaultMappings =
  [
    ("k", menuCycle 1),
    (withInsert "<c-k>", menuCycle 1),
    ("j", menuCycle (-1)),
    (withInsert "<c-j>", menuCycle (-1)),
    ("<space>", menuToggle),
    ("*", menuToggleAll),
    ("<esc>", menuQuit),
    ("<c-c>", menuQuit),
    (withInsert "<c-f>", menuCycleFilter)
  ]
