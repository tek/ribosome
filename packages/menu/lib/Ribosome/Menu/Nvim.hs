module Ribosome.Menu.Nvim where

import Control.Lens (use, view, views, (%=), (.=), (<.=), (^.))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT (runStateT))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (Sum, getSum))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text (cons, snoc)
import qualified Polysemy.Log as Log

import Ribosome.Api.Window (redraw, restoreView, setLine)
import Ribosome.Data.Scratch (Scratch (scratchWindow), scratchBuffer)
import Ribosome.Data.ScratchOptions (ScratchOptions, maxSize)
import Ribosome.Data.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch)
import Ribosome.Data.WindowView (PartialWindowView (PartialWindowView))
import Ribosome.Host.Api.Effect (nvimBufIsLoaded, nvimSetCurrentWin)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import Ribosome.Menu.Data.Menu (Menu)
import qualified Ribosome.Menu.Data.MenuData as Menu
import Ribosome.Menu.Data.MenuData (entries)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent (..))
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer (MenuRenderer))
import Ribosome.Menu.Data.MenuView (MenuView (MenuView), botIndex, cursorLine, menuView, topIndex)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState, cursorIndex, indexes)
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
  StateT NvimMenuState (ReaderT (Menu i) IO) [Entry i]
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
  Int ->
  StateT NvimMenuState (ReaderT (Menu i) IO) ([Entry i], Bool)
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
  StateT NvimMenuState (ReaderT (Menu i) IO) Int
windowLine = do
  top <- use topIndex
  bot <- use botIndex
  curLine <- use cursorLine
  pure (top - bot - fromIntegral curLine)

runSR ::
  Members [AtomicState s, Reader e, Embed IO] r =>
  StateT s (ReaderT e IO) a ->
  Sem r a
runSR ma = do
  e <- ask
  s <- atomicGet
  (a, s') <- embed (runReaderT (runStateT ma s) e)
  atomicPut s'
  pure a

-- TODO looks like the only thing needed from `Menu` here is `cursor`.
-- this obviates most of what `readMenuForRender` does, in particular `dirty` appears not to be used.
-- check whether `dirty` was just not implemented yet and might be useful. probably its function is now fulfilled by the
-- stream not producing menu events if nothing was changed
updateMenu ::
  Members [AtomicState NvimMenuState, Reader (Menu i), Rpc, Rpc !! RpcError, Log, Embed IO] r =>
  ScratchOptions ->
  Scratch ->
  Sem r ()
updateMenu options scratch = do
  nvimSetCurrentWin win
  (visible, changed) <- runSR (updateMenuState (fromMaybe 30 (options ^. maxSize)))
  when changed do
    setScratchContent options scratch (reverse (toList (withMark <$> visible)))
  restoreView (PartialWindowView Nothing (Just 1))
  targetLine <- runSR windowLine
  setLine win targetLine !>> Log.debug "menu cursor line invalid"
  where
    win =
      scratchWindow scratch

renderNvimMenu ::
  ∀ i r .
  Members [AtomicState NvimMenuState, Reader (Menu i), Rpc, Rpc !! RpcError, Log, Embed IO] r =>
  ScratchOptions ->
  Scratch ->
  Sem r ()
renderNvimMenu options scratch =
  whenM (nvimBufIsLoaded (scratchBuffer scratch)) do
    updateMenu options scratch
    redraw

nvimMenuRenderer ::
  Members [AtomicState NvimMenuState, Rpc, Rpc !! RpcError, AtomicState (Map Text Scratch), Log, Embed IO, Final IO] r =>
  ScratchOptions ->
  Scratch ->
  MenuRenderer r i
nvimMenuRenderer options scratch =
  MenuRenderer \ menu -> \case
  MenuRenderEvent.Render ->
    runReader menu (renderNvimMenu options scratch)
  MenuRenderEvent.Quit ->
    killScratch scratch
