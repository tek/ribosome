module Ribosome.Menu.Nvim where

import Control.Concurrent.Lifted (modifyMVar)
import Control.Lens (use, view, views, (%=), (.=), (<.=))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map (fromList)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text (cons, snoc)

import Ribosome.Api.Window (redraw, restoreView, setLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Scratch (Scratch (scratchWindow), scratchBuffer)
import Ribosome.Data.ScratchOptions (ScratchOptions, maxSize)
import Ribosome.Data.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch)
import Ribosome.Data.WindowView (PartialWindowView (PartialWindowView))
import Ribosome.Log (logDebug)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import Ribosome.Menu.Data.Menu (Menu)
import qualified Ribosome.Menu.Data.MenuData as Menu
import Ribosome.Menu.Data.MenuData (entries)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent (..))
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer (MenuRenderer))
import Ribosome.Menu.Data.MenuView (MenuView (MenuView), botIndex, cursorLine, topIndex, menuView)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState, cursorIndex, indexes)
import Ribosome.Nvim.Api.IO (nvimBufIsLoaded, nvimWinGetHeight)
import Ribosome.Nvim.Api.RpcCall (RpcError)
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

selectionLine :: SyntaxItem
selectionLine =
  item { siOptions = options }
  where
    item = syntaxMatch "RibosomeMenuMarkedLine" ".*$"
    options = ["contained"]

hlMarkedLine :: HiLink
hlMarkedLine =
  HiLink "RibosomeMenuMarkedLine" "Tag"

menuSyntax :: Syntax
menuSyntax =
  Syntax [markerConceal, selectionLine] [] [hlMarkedLine]

withMark :: Entry i -> Text
withMark (Entry (MenuItem _ _ text) _ sel) =
  bool id (Text.cons marker) sel text

entrySlice ::
  Entries i ->
  Int ->
  Int ->
  Seq (Entry i)
entrySlice ents bot top =
  fst (IntMap.foldr' f (Seq.empty, 0) ents)
  where
    f a (z, i) =
      if
        | i > top ->
          (z, newI)
        | i >= bot ->
          (z <> Seq.take (top - i + 1) a, newI)
        | newI >= bot ->
          (z <> Seq.take (top - bot + 1) (Seq.drop (bot - i) a), newI)
        | otherwise ->
          (z, newI)
      where
        newI =
          i  + length a

newEntrySlice ::
  MonadReader (Menu i) m =>
  MonadState NvimMenuState m =>
  m [Entry i]
newEntrySlice = do
  bot <- use botIndex
  top <- use topIndex
  ents <- view entries
  pure (toList (entrySlice ents bot top))

scrollDown ::
  CursorIndex ->
  Int ->
  Int ->
  MenuView
scrollDown newCursor@(CursorIndex bot) winHeight count =
  MenuView (min (bot + winHeight) count - 1) bot newCursor 0

scrollUp ::
  CursorIndex ->
  Int ->
  MenuView
scrollUp newCursor@(CursorIndex top) winHeight =
  MenuView top (max (top - winHeight + 1) 0) newCursor (fromIntegral winHeight - 1)

moveCursorLine ::
  CursorIndex ->
  Int ->
  Int ->
  MenuView ->
  MenuView
moveCursorLine newCursor winHeight count (MenuView _ oldBot oldCursor oldCurLine) =
  MenuView (min (oldBot + winHeight) count - 1) oldBot newCursor (oldCurLine + fromIntegral (newCursor - oldCursor))

resetView ::
  CursorIndex ->
  Int ->
  Int ->
  MenuView
resetView newCursor winHeight count =
  MenuView (min winHeight count - 1) 0 newCursor 0

computeView ::
  CursorIndex ->
  Int ->
  Int ->
  MenuView ->
  MenuView
computeView newCursor@(CursorIndex cur) maxHeight count nmenu@(MenuView _ oldBot _ _) =
  if
    | scrolledDown -> scrollDown newCursor maxHeight count
    | scrolledUp -> scrollUp newCursor maxHeight
    | otherwise -> moveCursorLine newCursor maxHeight count nmenu
  where
    scrolledUp =
      relativeTop < cur
    relativeTop =
      newBot + maxHeight - 1
    newBot =
      min oldBot cur
    scrolledDown =
      oldBot > cur

entryId :: Entry i -> (Int, Bool)
entryId (Entry _ i s) =
  (i, s)

updateMenuState ::
  MonadReader (Menu i) m =>
  MonadState NvimMenuState m =>
  Int ->
  m ([Entry i], Bool)
updateMenuState scratchMax = do
  oldIndexes <- use indexes
  newCursor <- view Menu.cursor
  count <- views entries (getSum . foldMap (Sum . Seq.length))
  menuView %= computeView newCursor (min count scratchMax) count
  cursorIndex .= newCursor
  visible <- newEntrySlice
  newIndexes <- indexes <.= (entryId <$> visible)
  pure (visible, newIndexes /= oldIndexes)

windowLine ::
  MonadState NvimMenuState m =>
  m Int
windowLine = do
  top <- use topIndex
  bot <- use botIndex
  curLine <- use cursorLine
  pure (top - bot - fromIntegral curLine)

updateMenu ::
  NvimE e m =>
  MonadRibo m =>
  ScratchOptions ->
  Scratch ->
  Int ->
  StateT NvimMenuState (ReaderT (Menu i) m) ()
updateMenu options scratch _ = do
  (visible, changed) <- updateMenuState (fromMaybe 30 (options ^. maxSize))
  when changed do
    setScratchContent options scratch (reverse (toList (withMark <$> visible)))
  restoreView (PartialWindowView Nothing (Just 1))
  targetLine <- windowLine
  catchAt @RpcError invalidCursor (setLine (scratchWindow scratch) targetLine)
  unit
  where
    invalidCursor _ =
      logDebug @Text "menu cursor line invalid"

renderNvimMenu ::
  ∀ i e m .
  NvimE e m =>
  MonadRibo m =>
  ScratchOptions ->
  Scratch ->
  StateT NvimMenuState (ReaderT (Menu i) m) ()
renderNvimMenu options scratch =
  whenM (nvimBufIsLoaded (scratchBuffer scratch)) do
    updateMenu options scratch =<< nvimWinGetHeight (scratchWindow scratch)
    -- logDebug @Text logMsg
    redraw
  where
    -- logMsg =
    --   [exon|menu cursor to #{show lineNumber}; #{show (menu ^. cursor)}/#{show (length items)}|]

nvimMenuRenderer ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  ScratchOptions ->
  Scratch ->
  m (MenuRenderer m i)
nvimMenuRenderer options scratch = do
  nvimState <- newMVar def
  pure $ MenuRenderer \case
    MenuRenderEvent.Render ->
      modifyMVar nvimState (fmap swap . runStateT (renderNvimMenu options scratch))
    MenuRenderEvent.Quit ->
      killScratch scratch
