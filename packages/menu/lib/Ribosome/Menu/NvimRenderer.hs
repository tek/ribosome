module Ribosome.Menu.NvimRenderer where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((<|))
import qualified Data.Text as Text
import Exon (exon)
import qualified Polysemy.Log as Log

import Ribosome.Api (nvimBufLineCount)
import Ribosome.Api.Window (redraw, setLine, windowExec)
import qualified Ribosome.Data.ScratchOptions
import qualified Ribosome.Data.ScratchState
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Data.Syntax.Syntax (HiLink (..), Syntax (Syntax))
import Ribosome.Data.SyntaxItem (SyntaxItem (..))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host.Api.Data (nvimBufIsLoaded, windowGetWidth)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.CursorLine (CursorLine (CursorLine))
import qualified Ribosome.Menu.Data.Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry), ItemIndex, entriesLength, entriesLineCount, entryLineCount)
import Ribosome.Menu.Data.MenuAction (RenderAnchor (AnchorIndex, AnchorLine))
import qualified Ribosome.Menu.Data.MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import qualified Ribosome.Menu.Data.MenuStatus as MenuStatus
import Ribosome.Menu.Data.MenuStatus (MenuStatus (MenuStatus))
import qualified Ribosome.Menu.Data.MenuView
import Ribosome.Menu.Data.MenuView (EntryIndex (EntryIndex), MenuView (MenuView), ViewRange (ViewRange))
import qualified Ribosome.Menu.Data.NvimMenuState
import Ribosome.Menu.Data.NvimMenuState (
  EntrySlice (EntrySlice, OnlyPartialEntry),
  NvimMenuState,
  PartialEntry (PartialEntry),
  SliceIndexes (SliceIndexes),
  sliceLength,
  sliceRange,
  )
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Integral (subClamp)
import Ribosome.Menu.Lens (view, (.=), (<.=))
import Ribosome.Syntax.Cons (syntaxMatch)

newtype AvailLines =
  AvailLines Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

-- TODO use signs instead of this
marker :: Char
marker = '†'

markerConceal :: SyntaxItem
markerConceal =
  item {options, params}
  where
    item = syntaxMatch "RibosomeMenuMarker" (Text.snoc "^" marker)
    options = ["conceal"]
    params = Map.fromList [("nextgroup", "RibosomeMenuMarkedLine")]

selectionLine :: SyntaxItem
selectionLine =
  item {options}
  where
    item = syntaxMatch "RibosomeMenuMarkedLine" ".*$"
    options = ["contained"]

-- TODO this appears not to be used, investigate
hlMarkedLine :: HiLink
hlMarkedLine =
  HiLink "RibosomeMenuMarkedLine" "Tag"

menuSyntax :: Syntax
menuSyntax =
  Syntax [markerConceal, selectionLine] [] [hlMarkedLine]

withMark :: Entry i -> NonEmpty Text
withMark (Entry (MenuItem _ _ text) _ sel)
  | sel
  , h :| t <- text
  =
    Text.cons marker h :| t
  | otherwise =
    text

renderPartial :: Bool -> Maybe (PartialEntry i) -> [NonEmpty Text]
renderPartial top = \case
  Just pe | h : t <- taker pe pe.entry.item.render -> [h :| t]
  _ -> []
  where
    taker PartialEntry {..} | top = NonEmpty.drop (fromIntegral (entry.item.lines - visibleLines))
                            | otherwise = NonEmpty.take (fromIntegral visibleLines)

renderSlice :: EntrySlice i -> [NonEmpty Text]
renderSlice = \case
  EntrySlice {..} ->
    renderPartial True partialTop <>
    (withMark <$> full) <>
    renderPartial False partialBot
  OnlyPartialEntry {..} ->
    renderPartial False (Just entry)

entryId :: Entry i -> (ItemIndex, Bool)
entryId (Entry _ i s) = (i, s)

partialEntryId :: PartialEntry i -> (ItemIndex, Word, Bool)
partialEntryId (PartialEntry (Entry _ i s) v) = (i, v, s)

type Acc i = (Maybe (Seq (Entry i), Maybe (PartialEntry i)), EntryIndex, Word)

-- | For Bot: Uses a right fold for the 'IntMap' because the highest score comes first in the UI, then takes the 'Seq'
-- from the left since these entries are ordered.
entrySlice ::
  ∀ i .
  (∀ a b . (b -> a -> b) -> b -> IntMap a -> b) ->
  (∀ a b . (b -> a -> b) -> b -> Seq a -> b) ->
  Entries i ->
  EntryIndex ->
  AvailLines ->
  Maybe (Seq (Entry i), Maybe (PartialEntry i))
entrySlice scoreFolder entryFolder ents start (AvailLines avail) =
  result
  where
    (result, _, _) = scoreFolder score (Nothing, 0, 0) ents

    score :: Acc i -> Seq (Entry i) -> Acc i
    score acc@(res, i, l) es

      | l >= avail = acc

      | let len = fromIntegral (Seq.length es)
            new = i + len
      , new <= start
      = (res, new, l)

      | otherwise = entryFolder entry acc es

    entry :: Acc i -> Entry i -> Acc i
    entry acc@(Just (_, Just _), _, _) _ = acc

    entry (res, i, l) e

      | i < start || l >= avail
      = (res, i + 1, l)

      | len <= remaining
      = (Just add, i + 1, l + len)

      | otherwise
      = (Just (usePartial (PartialEntry e remaining)), i + 1, avail)
      where
        len = fromIntegral (length e.item.render)

        remaining = subClamp avail l

        add | Just (es, p) <- res = (e <| es, p)
            | otherwise = ([e], Nothing)

        usePartial p | Just (es, _) <- res = (es, Just p)
                     | otherwise = (empty, Just p)

entrySliceBot ::
  Entries i ->
  EntryIndex ->
  AvailLines ->
  Maybe (EntrySlice i)
entrySliceBot ents indexBot avail =
  entrySlice (IntMap.foldr' . flip) foldl ents indexBot avail <&> \case
    (Seq.Empty, Just partialTop) ->
      OnlyPartialEntry {entry = partialTop, index = indexBot}
    (full, partialTop) ->
      let
        count = length full
        indexTop | count == 0 = indexBot
                | otherwise = indexBot + fromIntegral count - 1
      in
        EntrySlice {full = toList full, indexBot, partialBot = Nothing, ..}

entrySliceTop ::
  Entries i ->
  EntryIndex ->
  AvailLines ->
  Maybe (EntrySlice i)
entrySliceTop ents indexTop avail =
  entrySlice IntMap.foldl' (foldr' . flip) ents bot avail <&> \case
    (Seq.Empty, Just partialBot) ->
      OnlyPartialEntry {entry = partialBot, index = indexTop}
    (full, partialBot) ->
      let
        count = length full
        indexBot | count == 0 = indexTop
                | otherwise = indexTop - fromIntegral count + 1
      in
        EntrySlice {full = reverse (toList full), indexTop, partialTop = Nothing, ..}
    where
      bot = subClamp (EntryIndex total) (1 + indexTop)
      total = entriesLength ents

preferBottomPartial :: AvailLines -> EntryIndex -> PartialEntry i -> PartialEntry i -> EntrySlice i
preferBottomPartial (AvailLines avail) index bot top
  | bottomEntryFits =
    EntrySlice {
      full = [bot.entry],
      indexBot = index,
      indexTop = index,
      partialBot = Nothing,
      partialTop
    }
  | otherwise =
    OnlyPartialEntry {entry = PartialEntry {entry = bot.entry, visibleLines = avail}, index}
  where
    partialTop | botExtra == 0 = Nothing
               | otherwise = Just PartialEntry {entry = top.entry, visibleLines = avail - entryLineCount bot.entry}

    bottomEntryFits = botExtra >= 0

    botExtra :: Int
    botExtra = fromIntegral @Word @Int avail - fromIntegral @Word @Int (entryLineCount bot.entry)

-- | Fetch entries below the old cursor index to fill the space below the old cursor line, then fetch entries above the
-- cursor index to fill the remaining space.
-- This might return fewer lines than available if there are only a few visible items.
--
-- Uses the old cursor because we want to ensure that the items are displayed at the same position as before, which
-- usually happens when the cursor is moved within the range of displayed items.
entrySliceCursor ::
  Entries i ->
  CursorIndex ->
  CursorLine ->
  AvailLines ->
  Maybe (EntrySlice i)
entrySliceCursor ents cursor (CursorLine cursorLine) avail =
  (withBot <$> sliceBot) <|> onlyTop
  where

    onlyTop = sliceTop avail

    withBot slice =
      padBot (maybe slice (catSlices slice) (sliceTop topCount))
      where
        -- If a full slice and a partial item was found, there were enough items below the cursor to fill the space, so
        -- we can use the available bottom space (@== cursorLine + 1@) to calculate the top space.
        -- If not, we have to count all the lines in the returned entries.
        -- Same applies to the new cursor line – it moves to the number of bottom entry lines if no partial item was
        -- found.
        topCount = case slice of
          EntrySlice {partialBot = Just _} -> subClamp avail botAvail
          _ -> subClamp avail botLines

        botLines = case slice of
          EntrySlice {full} -> entriesLineCount full
          OnlyPartialEntry {entry} -> entry.visibleLines

    -- When an update causes the number of entries above the cursor to decrease below the available space, this function
    -- would return fewer lines than possible if there are more entries below the bottom of the range (when scrolling up
    -- before filtering).
    -- Therefore, we have to check whether the total line count is smaller than @avail@, and if so, prepend more
    -- entries.
    padBot e
      | Just diff <- tooSmall e
      , Just index <- canPad e
      =
        case entrySliceTop ents (subClamp @Word index 1) (fromIntegral diff) of
          Just new -> catSlices new e
          Nothing -> e

      | otherwise = e

    tooSmall e =
      justIf (diff > 0) diff
      where
        diff :: Int = fromIntegral avail - fromIntegral (sliceLength e)

    canPad = \case
      EntrySlice {partialTop = Nothing, indexBot} | indexBot > 0 ->
        Just (subClamp @Word indexBot 1)
      OnlyPartialEntry {index} -> Just index
      _ -> Nothing

    catSlices EntrySlice {full = entsBot, indexBot, partialBot} EntrySlice {full = entsTop, indexTop, partialTop} =
      EntrySlice {full = entsTop <> entsBot, ..}

    catSlices EntrySlice {full, indexBot, indexTop, partialBot} OnlyPartialEntry {entry} =
      EntrySlice {partialTop = Just entry, ..}

    catSlices OnlyPartialEntry {entry} EntrySlice {full, indexBot, indexTop, partialTop} =
      EntrySlice {partialBot = Just entry, ..}

    catSlices OnlyPartialEntry {entry = bot, index} OnlyPartialEntry {entry = top} =
      preferBottomPartial avail index bot top

    sliceTop topCount = entrySliceBot ents (fromIntegral (cursor + 1)) topCount

    sliceBot = entrySliceTop ents (fromIntegral cursor) botAvail

    botAvail = AvailLines (cursorLine + 1)

data ViewChange =
  ScrollUp
  |
  ScrollDown
  |
  AtLine CursorLine
  deriving stock (Eq, Show, Generic)

entrySliceForChange ::
  Entries i ->
  CursorIndex ->
  AvailLines ->
  ViewChange ->
  Maybe (EntrySlice i)
entrySliceForChange ents newCursor avail = \case
  ScrollUp -> entrySliceTop ents (fromIntegral newCursor) avail
  ScrollDown -> entrySliceBot ents (fromIntegral newCursor) avail
  AtLine cursorLine -> entrySliceCursor ents newCursor cursorLine avail

bestLineForIndex :: EntryIndex -> [(EntryIndex, CursorLine)] -> CursorLine
bestLineForIndex (fromIntegral -> target) =
  snd . foldl' step (Nothing :: Maybe Int, 0)
  where
    step (best, bestLine) (index, entryLine)
      | let diff = abs (target - fromIntegral index)
      , better diff best
      = (Just diff, entryLine)

      | otherwise
      = (best, bestLine)

    better diff = \case
      Nothing -> True
      Just best -> diff < best

viewChange :: RenderAnchor -> Maybe ViewRange -> CursorIndex -> ViewChange

viewChange AnchorIndex (Just ViewRange {..}) (fromIntegral -> newCursor)
  | newCursor > top = ScrollUp
  | newCursor < bottom = ScrollDown
  | otherwise = AtLine (bestLineForIndex newCursor entryLines)

viewChange AnchorLine (Just ViewRange {cursorLine}) _ =
  AtLine cursorLine

viewChange _ Nothing _ = ScrollDown

sliceIndexes :: EntrySlice i -> SliceIndexes
sliceIndexes = \case
  EntrySlice {..} ->
    SliceIndexes {
      full = entryId <$> full,
      partialBot = partialEntryId <$> partialBot,
      partialTop = partialEntryId <$> partialTop
    }
  OnlyPartialEntry {..} ->
    SliceIndexes {
      full = [],
      partialBot = Just (partialEntryId entry),
      partialTop = Nothing
    }

-- | Given a cursor index, top index, and list of entries, determine the line number of the first line in the entry
-- list when displayed in the UI:
--
-- 1. Use the visible lines of a potential partial bottom entry as base offset, or 0.
-- 2. Compute the relative index of the cursor from the top (both cursor and bottom index are absolute over all visible
--    entries).
-- 3. Sum the lines of the entries from the bottom up to, including, the entry pointed to by the relative index, by
--    dropping the number of entries given by the relative index from 2.
-- 4. Add the base offset.
-- 5. Subtract one to get a zero-based line number.
findCursorLine :: CursorIndex -> EntrySlice i -> CursorLine
findCursorLine newCursor = \case
  EntrySlice {..} ->
    CursorLine (subClamp @Word (linesBelowCursor + partialOffset) 1)
    where
      linesBelowCursor :: Word
      linesBelowCursor = sum (entryLineCount <$> entriesBelowCursor)

      entriesBelowCursor = drop relative full

      relative :: Int
      relative = fromIntegral (subClamp indexTop newCursor)

      partialOffset :: Word
      partialOffset = maybe 0 (.visibleLines) partialBot
  OnlyPartialEntry {entry} -> CursorLine (subClamp @Word entry.visibleLines 1)

sliceEntryLines :: EntrySlice i -> [(EntryIndex, CursorLine)]
sliceEntryLines EntrySlice {..} =
  snd (mapAccumL step (indexBot, partialOffset) full)
  where
    step (i, z) e =
      ((i + 1, new), (i, CursorLine (subClamp @Word new 1)))
      where
        new = z + e.item.lines

    partialOffset :: Word
    partialOffset = maybe 0 (.visibleLines) partialBot

sliceEntryLines OnlyPartialEntry {index, entry} =
  [(index, CursorLine (subClamp @Word entry.entry.item.lines 1))]

-- TODO remember to fix the correctness of the cursor when this is done:
-- when items are refined, the cursor may become out of bounds, but it is not corrected, since it isn't even available
-- at that location.
-- this causes the view to become entirely wrong, pressing <cr> for an action does nothing at all
--
-- TODO When the new slice is empty, the range has to be reset.
-- As it is, filtering until no items are matched and backspacing will start with a bad range.
updateMenuState ::
  Member (State NvimMenuState) r =>
  RenderAnchor ->
  Entries i ->
  CursorIndex ->
  AvailLines ->
  Sem r (Maybe (EntrySlice i, Bool))
updateMenuState anchor ents newCursor scratchMax = do
  old <- get
  let change = viewChange anchor old.view.range newCursor
      maybeSlice = entrySliceForChange ents newCursor scratchMax change
  for maybeSlice \ slice -> do
    let
      cursorLine = findCursorLine newCursor slice
      (bottom, top) = sliceRange slice
      entryLines = sliceEntryLines slice
    #view .= MenuView (Just ViewRange {bottom, top, cursorLine, entryLines}) newCursor
    newIndexes <- #slice <.= (sliceIndexes slice)
    pure (slice, newIndexes /= old.slice)

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
    newCursor <- view #cursor
    ents <- view #entries
    anchor <- view #anchor
    maybe clear update =<< updateMenuState anchor ents newCursor (fromMaybe 30 maxSize)
  where
    maxSize = fromIntegral <$> scratch.options.maxSize

    clear = void (Scratch.update scratch.id (mempty @[_]))

    update (visible, changed) = do
      when changed do
        void (Scratch.update scratch.id (toList =<< renderSlice visible))
      gets (.view.range) >>= traverse_ \ range -> do
        alignContent
        count <- nvimBufLineCount scratch.buffer
        let windowLine = subClamp count (1 + range.cursorLine)
        setLine scratch.window windowLine !>> Log.debug "menu cursor line invalid"

    -- Even though the line count is always matched to the window size, it is possible for the content to be scrolled
    -- out of the viewport.
    -- This enforces that the first content line is anchored at the first UI line.
    alignContent = windowExec scratch.window "call winrestview({'topline': 1})"

updateStatus ::
  Members [Reader (RenderMenu i), Scratch !! RpcError, Rpc, Stop RpcError] r =>
  ScratchState ->
  Sem r ()
updateStatus scr = do
  width <- windowGetWidth scr.window
  view #status >>= \ MenuStatus {filter = f, middle, ..} -> do
    let
      builtinSegmentsSize =
        Text.length filterSegment + Text.length countSegment
      space =
        subClamp width (2 :: Word)
      middleSpace =
        subClamp space builtinSegmentsSize
      filterSegment =
        [exon|🔎 #{f}|]
      middleSegment =
        Text.take middleSpace (fromMaybe "" (middle middleSpace))
      bottomSegment =
        Text.take space <$> bottom space
      countSegment =
        [exon|#{show (cursor + 1)}/#{show entryCount}/#{show itemCount}|]
      gutter =
        space - (builtinSegmentsSize + Text.length middleSegment)
      leftGutter =
        fromMaybe 0 (div gutter 2)
      rightGutter =
        subClamp gutter leftGutter
      ws n =
        Text.replicate n " "
      segments =
        if gutter <= 0
        then countSegment
        else [exon|#{filterSegment}#{ws leftGutter}#{middleSegment}#{ws rightGutter}#{countSegment}|]
    void $ restop $ Scratch.update (scr ^. #id) (segments : bottomSegment)

renderNvimMenu ::
  ∀ i r .
  Member (AtomicState NvimMenuState) r =>
  Members [Reader (RenderMenu i), Rpc !! RpcError, Scratch !! RpcError, Log, Stop RpcError, Embed IO] r =>
  ScratchState ->
  Maybe ScratchState ->
  Sem r ()
renderNvimMenu itemsScratch statusScratch =
  restop @_ @Rpc $ restop @_ @Scratch do
    whenM (nvimBufIsLoaded itemsScratch.buffer) do
      updateMenu itemsScratch
      for_ statusScratch \ s ->
        whenM (nvimBufIsLoaded s.buffer) do
          updateStatus s
      redraw
