module Ribosome.Menu.Nvim where

import Control.Lens (view)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (cons, snoc)

import Ribosome.Api.Window (redraw, setLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.List (mapSelectors)
import Ribosome.Data.Scratch (Scratch(scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.Syntax (
  HiLink(..),
  Syntax(Syntax),
  SyntaxItem(..),
  syntaxMatch,
  )
import Ribosome.Log (logDebug)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent(..))
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
renderNvimMenu options scratch (MenuRenderEvent.Render changed (Menu _ allItems selected marked _ maxItems)) =
  when changed (setScratchContent options scratch (reverse text)) *>
  logDebug @Text logMsg *>
  updateCursor *>
  redraw
  where
    lineNumber =
      max 0 $ length items - selected - 1
    text =
      withMarks marked (view MenuItem.text <$> items)
    items =
      limit allItems
    limit =
      maybe id take maxItems
    updateCursor =
      setLine (scratchWindow scratch) lineNumber
    logMsg =
      "updating menu cursor to line " <> show lineNumber <> "; " <> show selected <> "/" <> show (length items)
