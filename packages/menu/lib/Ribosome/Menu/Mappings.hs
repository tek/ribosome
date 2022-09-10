module Ribosome.Menu.Mappings where

import Ribosome.Data.Mapping (MapMode (MapInsert), MappingSpec)
import Ribosome.Menu.Action (MenuWidget, menuCycle, menuQuit, menuToggle, menuToggleAll)

type Mappings f i r a =
  Map MappingSpec (MenuWidget f i r a)

insert :: MappingSpec -> MappingSpec
insert =
  #mode %~ ([MapInsert] <>)

insertMapping :: MappingSpec -> MappingSpec
insertMapping =
  #mode .~ [MapInsert]

defaultMappings :: Mappings f i r a
defaultMappings =
  [
    ("k", menuCycle 1),
    (insert "<c-k>", menuCycle 1),
    ("j", menuCycle (-1)),
    (insert "<c-j>", menuCycle (-1)),
    ("<space>", menuToggle),
    ("*", menuToggleAll),
    ("<esc>", menuQuit),
    ("<c-c>", menuQuit)
  ]
