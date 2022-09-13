module Ribosome.Menu.Mappings where

import Ribosome.Data.Mapping (MapMode (MapInsert), MappingSpec)
import Ribosome.Menu.Action (MenuWidget, menuCycle, menuCycleFilter, menuQuit, menuToggle, menuToggleAll)
import Ribosome.Menu.Class.MenuState (MenuState)

type Mappings s r a =
  Map MappingSpec (MenuWidget s r a)

withInsert :: MappingSpec -> MappingSpec
withInsert =
  #mode %~ ([MapInsert] <>)

insert :: MappingSpec -> MappingSpec
insert =
  #mode .~ [MapInsert]

defaultMappings ::
  MenuState s =>
  Mappings s r a
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
