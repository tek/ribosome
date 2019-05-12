module Ribosome.Menu.Nvim where

import Ribosome.Api.Buffer (setBufferContent)
import Ribosome.Api.Window (setLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Scratch (Scratch(Scratch, scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Nvim.Api.IO (windowSetHeight)
import Ribosome.Scratch (setScratchContent)

renderNvimMenu ::
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  Scratch ->
  MenuUpdate ->
  m ()
renderNvimMenu options scratch (MenuUpdate event (Menu _ allItems _ selected)) = do
  setScratchContent options scratch (reverse text)
  setLine (scratchWindow scratch) (length items - selected - 1)
  where
    size = min (length items) 20
    text = MenuItem.text <$> items
    items = take maxItems (reverse allItems)
    maxItems = 100
