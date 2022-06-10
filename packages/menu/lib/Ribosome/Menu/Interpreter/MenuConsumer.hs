module Ribosome.Menu.Interpreter.MenuConsumer where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))

import Ribosome.Menu.Action (menuCycle, menuToggle, menuToggleAll)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuState (MenuStack, MenuWidget)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent))

type Mappings r a =
  Map Text (MenuWidget r a)

defaultMappings ::
  Members (MenuStack i) r =>
  Mappings r a
defaultMappings =
  Map.fromList [
    ("k", menuCycle 1),
    ("c-k", menuCycle 1),
    ("j", menuCycle (-1)),
    ("c-j", menuCycle (-1)),
    ("space", menuToggle),
    ("*", menuToggleAll)
  ]

forMappings ::
  ∀ a r .
  Mappings r a ->
  InterpreterFor (MenuConsumer a) r
forMappings mappings =
  interpret \case
    MenuConsumerEvent (MenuEvent.Mapping char) -> do
      let
      fromMaybe (pure Nothing) (mappings !? char)
    MenuConsumerEvent _ ->
      pure Nothing

withMappings ::
  ∀ i a r .
  Members (MenuStack i) r =>
  Mappings r a ->
  InterpreterFor (MenuConsumer a) r
withMappings extraMappings =
  forMappings (Map.union extraMappings defaultMappings)

basic ::
  ∀ a i r .
  Members (MenuStack i) r =>
  InterpreterFor (MenuConsumer a) r
basic =
  withMappings mempty
