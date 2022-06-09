module Ribosome.Menu.NvimRenderer where

import Control.Lens (use, view, views, (%=), (.=), (<.=), (^.))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT (runStateT))
import Data.Generics.Labels ()
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (Sum, getSum))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text (cons, snoc)
import qualified Polysemy.Log as Log

import Ribosome.Api.Window (redraw, restoreView, setLine)
import Ribosome.Data.ScratchState (ScratchState (buffer, window))
import Ribosome.Data.WindowView (PartialWindowView (PartialWindowView))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host.Api.Effect (nvimBufIsLoaded, nvimSetCurrentWin)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Data.MenuView (MenuView (MenuView))
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState, botIndex, cursorLine, topIndex)
import Ribosome.Menu.ItemLens (cursor, entries)
import Ribosome.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch)

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
  StateT NvimMenuState (ReaderT (Menu i) Identity) [Entry i]
newEntrySlice = do
  bot <- use (#view . #botIndex)
  top <- use (#view . #topIndex)
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
  StateT NvimMenuState (ReaderT (Menu i) Identity) ([Entry i], Bool)
updateMenuState scratchMax = do
  oldIndexes <- use #indexes
  newCursor <- view cursor
  count <- views entries (getSum . foldMap (Sum . Seq.length))
  #view %= computeView newCursor (min count scratchMax) count
  #cursorIndex .= newCursor
  visible <- newEntrySlice
  newIndexes <- #indexes <.= (entryId <$> visible)
  pure (visible, newIndexes /= oldIndexes)

windowLine ::
  StateT NvimMenuState (ReaderT (Menu i) Identity) Int
windowLine = do
  top <- use topIndex
  bot <- use botIndex
  curLine <- use cursorLine
  pure (top - bot - fromIntegral curLine)

runSR ::
  Members [AtomicState s, Reader e] r =>
  StateT s (ReaderT e Identity) a ->
  Sem r a
runSR ma = do
  e <- ask
  s <- atomicGet
  let (a, s') = runIdentity (runReaderT (runStateT ma s) e)
  atomicPut s'
  pure a

-- TODO looks like the only thing needed from `Menu` here is `cursor`.
-- this obviates most of what `readMenuForRender` does, in particular `dirty` appears not to be used.
-- check whether `dirty` was just not implemented yet and might be useful. probably its function is now fulfilled by the
-- stream not producing menu events if nothing was changed
updateMenu ::
  Members [Scratch, AtomicState NvimMenuState, Reader (Menu i), Rpc, Rpc !! RpcError, Log] r =>
  ScratchState ->
  Sem r ()
updateMenu scratch = do
  nvimSetCurrentWin win
  (visible, changed) <- runSR (updateMenuState (fromMaybe 30 (scratch ^. #options . #maxSize)))
  when changed do
    void (Scratch.update (scratch ^. #id) (reverse (toList (withMark <$> visible))))
  restoreView (PartialWindowView Nothing (Just 1))
  targetLine <- runSR windowLine
  setLine win targetLine !>> Log.debug "menu cursor line invalid"
  where
    win =
      window scratch

renderNvimMenu ::
  ∀ i r .
  Members [Scratch, AtomicState NvimMenuState, Reader (Menu i), Rpc, Rpc !! RpcError, Log, Embed IO] r =>
  ScratchState ->
  Sem r ()
renderNvimMenu scratch =
  whenM (nvimBufIsLoaded (buffer scratch)) do
    updateMenu scratch
    redraw
