module Ribosome.Menu.Nvim where

import Ribosome.Api.Window (setLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Scratch (Scratch(scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(Quit))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Scratch (killScratch, setScratchContent)

renderNvimMenu ::
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  Scratch ->
  MenuUpdate m a ->
  m ()
renderNvimMenu _ scratch (MenuUpdate (MenuEvent.Quit _) _) =
  killScratch scratch
renderNvimMenu options scratch (MenuUpdate _ (Menu _ allItems _ selected _)) = do
  setScratchContent options scratch (reverse text)
  setLine (scratchWindow scratch) (max 0 $ length items - selected - 1)
  vimCommand "redraw"
  where
    text = MenuItem._text <$> items
    items = take maxItems (reverse allItems)
    maxItems = 100
