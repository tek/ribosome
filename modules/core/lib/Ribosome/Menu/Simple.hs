module Ribosome.Menu.Simple where

import Ribosome.Menu.Data.MenuContent (MenuContent)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Data.Prompt (Prompt(Prompt))

simpleMenu ::
  Monad m =>
  (MenuUpdate -> m ()) ->
  MenuUpdate ->
  m ()
simpleMenu consumer update@(MenuUpdate event _ (Prompt _ _ _)) =
  run event
  where
    run (MenuEvent.Character _) =
      consumer update
    run MenuEvent.Quit =
      return ()

renderNvimMenu :: MenuContent -> m ()
renderNvimMenu =
  undefined

-- simpleNvimMenu ::
--   Monad m =>
--   MenuUpdate ->
--   m ()
-- simpleNvimMenu =
--   simpleMenu renderNvimMenu
