module Ribosome.Menu.Data.MenuConsumer where

import Ribosome.Menu.Data.MenuAction (MenuAction, hoistMenuAction)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuState (MenuStateSem)

type MenuWidget i r a =
  MenuStateSem i r (Maybe (MenuAction r a))

newtype MenuApp r i a =
  MenuApp { unMenuApp :: MenuEvent -> MenuWidget i r a }

newtype MenuConsumer r i a =
  MenuConsumer { unMenuConsumer :: MenuEvent -> MenuWidget i r a }

hoistMenuConsumer ::
  (∀ x . Sem r x -> Sem r' x) ->
  (∀ x . MenuStateSem i r x -> MenuStateSem i r' x) ->
  MenuConsumer r i a ->
  MenuConsumer r' i a
hoistMenuConsumer f g (MenuConsumer con) =
  MenuConsumer \ e -> fmap (hoistMenuAction f) <$> g (con e)
