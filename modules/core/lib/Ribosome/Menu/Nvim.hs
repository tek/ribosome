module Ribosome.Menu.Nvim where

import Ribosome.Api.Window (redraw, setLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Scratch (Scratch(scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Log (logDebug)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConsumerUpdate (MenuConsumerUpdate(MenuConsumerUpdate))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(Quit))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Scratch (killScratch, setScratchContent)

renderNvimMenu ::
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  Scratch ->
  MenuRenderEvent m a ->
  m ()
renderNvimMenu _ scratch (MenuRenderEvent.Quit _) =
  killScratch scratch
renderNvimMenu options scratch (MenuRenderEvent.Render changed (Menu _ allItems _ selected _)) = do
  when changed $ setScratchContent options scratch (reverse text)
  updateCursor
  redraw
  where
    lineNumber =
      max 0 $ length items - selected - 1
    text = MenuItem._text <$> items
    items = take maxItems (reverse allItems)
    maxItems = 100
    updateCursor = do
      logDebug @Text logMsg
      setLine (scratchWindow scratch) lineNumber
    logMsg =
      "updating menu cursor to line " <> show lineNumber <> "; " <> show selected <> "/" <> show (length items)
