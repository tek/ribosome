module Ribosome.Menu.Consumer where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (fromList, union)

import Ribosome.Menu.Action (menuCycle, menuToggle, menuToggleAll)
import Ribosome.Menu.Data.MenuConsumer (MenuApp (MenuApp), MenuConsumer (MenuConsumer), MenuWidget)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent (..))

type Mappings m i a =
  Map Text (MenuWidget m i a)

forApp ::
  MenuApp m i a ->
  MenuConsumer m i a
forApp (MenuApp consumer) =
  MenuConsumer \ menu event ->
    runReaderT (consumer event) menu

forMappings ::
  Applicative m =>
  Mappings m i a ->
  MenuConsumer m i a
forMappings mappings =
  forApp $ MenuApp \case
    MenuEvent.Mapping char ->
      fromMaybe (pure Nothing) (mappings !? char)
    _ ->
      pure Nothing

defaultMappings ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Mappings m i a
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
  MonadIO m =>
  MonadBaseControl IO m =>
  Mappings m i a ->
  MenuConsumer m i a
withMappings extraMappings =
  forMappings (Map.union extraMappings defaultMappings)

basic ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuConsumer m i a
basic =
  withMappings mempty
