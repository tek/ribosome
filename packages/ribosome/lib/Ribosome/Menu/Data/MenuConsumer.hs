module Ribosome.Menu.Data.MenuConsumer where

import Ribosome.Menu.Data.MenuAction (MenuAction, hoistMenuAction)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuState (MenuM, MenuStateM)
import Ribosome.Menu.Data.MenuStateSem (MenuStateSem)

type MenuWidget r m i a =
  MenuStateM m i (Maybe (MenuAction r a))

type MenuWidgetSem r i a =
  MenuStateSem r i (Maybe (MenuAction r a))

type MenuWidgetM r m i a =
  MenuM m i (Maybe (MenuAction r a))

newtype MenuApp r i a =
  MenuApp { unMenuApp :: MenuEvent -> MenuWidgetSem r i a }

newtype MenuConsumer r i a =
  MenuConsumer { unMenuConsumer :: MenuEvent -> MenuWidgetSem r i a }

hoistMenuConsumer ::
  (∀ x . Sem r x -> Sem r' x) ->
  (∀ x . MenuStateSem r i x -> MenuStateSem r' i x) ->
  MenuConsumer r i a ->
  MenuConsumer r' i a
hoistMenuConsumer f g (MenuConsumer con) =
  MenuConsumer \ e -> fmap (hoistMenuAction f) <$> g (con e)
