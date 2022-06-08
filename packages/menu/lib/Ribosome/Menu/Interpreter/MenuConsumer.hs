module Ribosome.Menu.Interpreter.MenuConsumer where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))

import Ribosome.Menu.Action (menuCycle, menuToggle, menuToggleAll)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuState (MenuStateStack, MenuWidget)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (MenuConsumerEvent))

type Mappings i r a =
  Map Text (MenuWidget i r a)

defaultMappings ::
  Members [Resource, Embed IO] r =>
  Mappings i r a
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
  ∀ i a r .
  Mappings i r a ->
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ r)
forMappings mappings =
  interpret \case
    MenuConsumerEvent (MenuEvent.Mapping char) -> do
      let
      fromMaybe (pure Nothing) (mappings !? char)
    MenuConsumerEvent _ ->
      pure Nothing

withMappings ::
  ∀ i a r .
  Members [Resource, Embed IO] r =>
  Mappings i r a ->
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ r)
withMappings extraMappings =
  forMappings (Map.union extraMappings defaultMappings)

basic ::
  ∀ i a r .
  Members [Resource, Embed IO] r =>
  InterpreterFor (MenuConsumer a) (MenuStateStack i ++ r)
basic =
  withMappings mempty

interpretMenuConsumer :: m ()
interpretMenuConsumer =
  undefined
