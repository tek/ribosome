module Ribosome.Menu.Nvim where

import Control.Lens (view)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (cons, snoc)

import Ribosome.Api.Window (redraw, restoreView, saveView, setLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.List (mapSelectors)
import Ribosome.Data.Scratch (Scratch (scratchWindow), scratchBuffer)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.Syntax (
  HiLink (..),
  Syntax (Syntax),
  SyntaxItem (..),
  syntaxMatch,
  )
import qualified Ribosome.Data.WindowView as WindowView (WindowView (..))
import Ribosome.Data.WindowView (PartialWindowView (PartialWindowView))
import Ribosome.Log (logDebug)
import qualified Ribosome.Menu.Data.FilteredMenuItem as FilteredMenuItem (item)
import Ribosome.Menu.Data.Menu (Menu (Menu), current)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (abbreviated)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent (..))
import Ribosome.Nvim.Api.IO (nvimBufIsLoaded, nvimSetCurrentWin, nvimWinGetHeight)
import Ribosome.Scratch (killScratch, setScratchContent)

marker :: Char
marker =
  '†'

markerConceal :: SyntaxItem
markerConceal =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "RibosomeMenuMarker" (Text.snoc "^" marker)
    options = ["conceal"]
    params = Map.fromList [("nextgroup", "RibosomeMenuMarkedLine")]

markedLine :: SyntaxItem
markedLine =
  item { siOptions = options }
  where
    item = syntaxMatch "RibosomeMenuMarkedLine" ".*$"
    options = ["contained"]

hlMarkedLine :: HiLink
hlMarkedLine =
  HiLink "RibosomeMenuMarkedLine" "Tag"

menuSyntax :: Syntax
menuSyntax =
  Syntax [markerConceal, markedLine] [] [hlMarkedLine]

withMarks :: [Int] -> [Text] -> [Text]
withMarks =
  mapSelectors (Text.cons marker)

renderNvimMenu ::
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  Scratch ->
  MenuRenderEvent m a i ->
  m ()
renderNvimMenu _ scratch (MenuRenderEvent.Quit _) =
  killScratch scratch
renderNvimMenu options scratch (MenuRenderEvent.Render changed menu@(Menu _ _ _ selected marked _ maxItems)) =
  whenM (nvimBufIsLoaded (scratchBuffer scratch)) do
    when changed (setScratchContent options scratch (reverse text))
    logDebug @Text logMsg
    updateCursor
    nvimSetCurrentWin win
    height <- nvimWinGetHeight win
    adjustTopline height
    redraw
  where
    lineNumber =
      max 0 $ length items - selected - 1
    text =
      withMarks marked (view (FilteredMenuItem.item . MenuItem.abbreviated) <$> items)
    items =
      limit (menu ^. current)
    limit =
      maybe id take maxItems
    updateCursor =
      setLine win lineNumber
    win =
      scratchWindow scratch
    logMsg =
      "updating menu cursor to line " <> show lineNumber <> "; " <> show selected <> "/" <> show (length items)
    adjustTopline height = do
      when (lineNumber > lastTopline) do
        topline <- WindowView.topline <$> saveView
        when (topline - 1 > lastTopline) do
          restoreView (PartialWindowView Nothing (Just (lastTopline + 1)))
          updateCursor
      where
        lastTopline =
          length items - height
