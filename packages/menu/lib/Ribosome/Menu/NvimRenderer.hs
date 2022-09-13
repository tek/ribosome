module Ribosome.Menu.NvimRenderer where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Exon (exon)
import qualified Polysemy.Log as Log

import Ribosome.Api.Window (redraw, setLine, windowExec)
import Ribosome.Data.ScratchState (ScratchState (buffer, window))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host.Api.Effect (nvimBufIsLoaded, windowGetWidth)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), entriesLength)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import qualified Ribosome.Menu.Data.MenuStatus as MenuStatus
import Ribosome.Menu.Data.MenuStatus (MenuStatus (MenuStatus))
import Ribosome.Menu.Data.MenuView (MenuView (MenuView))
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState, botIndex, cursorLine, topIndex)
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Lens (use, view, (%=), (.=), (<.=))
import Ribosome.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch)

-- TODO use signs instead of this
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
  Members [State NvimMenuState, Reader (RenderMenu i)] r =>
  Sem r [Entry i]
newEntrySlice = do
  bot <- use (#view . #botIndex)
  top <- use (#view . #topIndex)
  ents <- view #entries
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
  Members [State NvimMenuState, Reader (RenderMenu i)] r =>
  Int ->
  Sem r ([Entry i], Bool)
updateMenuState scratchMax = do
  oldIndexes <- use #indexes
  newCursor <- view #cursor
  count <- view (#entries . to entriesLength)
  #view %= computeView newCursor (min count scratchMax) count
  #cursorIndex .= newCursor
  visible <- newEntrySlice
  newIndexes <- #indexes <.= (entryId <$> visible)
  pure (visible, newIndexes /= oldIndexes)

windowLine ::
  Members [State NvimMenuState, Reader (RenderMenu i)] r =>
  Sem r Int
windowLine = do
  top <- use topIndex
  bot <- use botIndex
  curLine <- use cursorLine
  pure (top - bot - fromIntegral curLine)

runS ::
  Member (AtomicState s) r =>
  InterpreterFor (State s) r
runS ma = do
  s <- atomicGet
  (s', a) <- runState s ma
  a <$ atomicPut s'

updateMenu ::
  Members [Scratch, AtomicState NvimMenuState, Reader (RenderMenu i), Rpc, Rpc !! RpcError, Log] r =>
  ScratchState ->
  Sem r ()
updateMenu scratch =
  runS do
    (visible, changed) <- updateMenuState (fromMaybe 30 (scratch ^. #options . #maxSize))
    when changed do
      void (Scratch.update (scratch ^. #id) (reverse (toList (withMark <$> visible))))
    windowExec win "call winrestview({'topline': 1})"
    targetLine <- windowLine
    setLine win targetLine !>> Log.debug "menu cursor line invalid"
  where
    win =
      window scratch

updateStatus ::
  Members [Reader (RenderMenu i), Scratch !! RpcError, Rpc, Stop RpcError] r =>
  ScratchState ->
  Sem r ()
updateStatus scr = do
  width <- windowGetWidth (scr ^. #window)
  view #status >>= \ MenuStatus {filter = f, extra, ..} -> do
    let
      filterSegment =
        [exon|🔎 #{f}|]
      extraSegment =
        fromMaybe "" extra
      countSegment =
        [exon|#{show (cursor + 1)}/#{show entryCount}/#{show itemCount}|]
      gutter =
        width - (Text.length filterSegment + Text.length countSegment + Text.length extraSegment) - 2
      leftGutter =
        fromMaybe 0 (div gutter 2)
      rightGutter =
        gutter - leftGutter
      ws n =
        Text.replicate n " "
      segments =
        if gutter <= 0
        then countSegment
        else [exon|#{filterSegment}#{ws leftGutter}#{extraSegment}#{ws rightGutter}#{countSegment}|]
    void $ restop $ Scratch.update (scr ^. #id) ([segments] :: [Text])

renderNvimMenu ::
  ∀ i r .
  Member (AtomicState NvimMenuState) r =>
  Members [Reader (RenderMenu i), Rpc !! RpcError, Scratch !! RpcError, Log, Stop RpcError, Embed IO] r =>
  ScratchState ->
  Maybe ScratchState ->
  Sem r ()
renderNvimMenu itemsScratch statusScratch =
  restop @_ @Rpc $ restop @_ @Scratch do
    whenM (nvimBufIsLoaded (buffer itemsScratch)) do
      updateMenu itemsScratch
      for_ statusScratch \ s ->
        whenM (nvimBufIsLoaded (s ^. #buffer)) do
          updateStatus s
      redraw
