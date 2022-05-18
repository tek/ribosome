module Ribosome.Menu.Consumer where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (fromList, union)

import Ribosome.Menu.Action (menuCycle, menuToggle, menuToggleAll)
import Ribosome.Menu.Data.MenuConsumer (MenuApp (MenuApp), MenuConsumer (MenuConsumer), MenuWidgetSem)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent (..))

type Mappings r i a =
  Map Text (MenuWidgetSem r i a)

forApp ::
  MenuApp r i a ->
  MenuConsumer r i a
forApp (MenuApp consumer) =
  MenuConsumer consumer

forMappings ::
  Mappings r i a ->
  MenuConsumer r i a
forMappings mappings =
  forApp $ MenuApp \case
    MenuEvent.Mapping char ->
      fromMaybe (pure Nothing) (mappings !? char)
    _ ->
      pure Nothing

defaultMappings ::
  Members [Resource, Embed IO] r =>
  Mappings r i a
defaultMappings =
  Map.fromList [
    ("k", menuCycle 1),
    ("c-k", menuCycle 1),
    ("j", menuCycle (-1)),
    ("c-j", menuCycle (-1)),
    ("space", menuToggle),
    ("*", menuToggleAll)
  ]

withMappings ::
  Members [Resource, Embed IO] r =>
  Mappings r i a ->
  MenuConsumer r i a
withMappings extraMappings =
  forMappings (Map.union extraMappings defaultMappings)

basic ::
  Members [Resource, Embed IO] r =>
  MenuConsumer r i a
basic =
  withMappings mempty
