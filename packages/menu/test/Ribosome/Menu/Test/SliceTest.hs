module Ribosome.Menu.Test.SliceTest where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Test (Hedgehog, UnitTest, assertJust, evalMaybe, (===))

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.CursorLine (CursorLine)
import qualified Ribosome.Menu.Data.Entry as Entries
import Ribosome.Menu.Data.Entry (Entries, Entry)
import qualified Ribosome.Menu.Data.MenuItem
import qualified Ribosome.Menu.Data.MenuView
import Ribosome.Menu.Data.MenuView (EntryIndex, MenuView (MenuView), ViewRange (ViewRange))
import qualified Ribosome.Menu.Data.NvimMenuState
import Ribosome.Menu.Data.NvimMenuState (
  EntrySlice (EntrySlice, OnlyPartialEntry),
  NvimMenuState (NvimMenuState),
  PartialEntry (PartialEntry), sliceRange,
  )
import Ribosome.Menu.NvimRenderer (
  AvailLines,
  entrySliceBot,
  entrySliceCursor,
  entrySliceTop,
  findCursorLine,
  updateMenuState,
  )
import Test.Tasty (TestTree, testGroup)
import Zeugma (unitTest)
import Zeugma (runTest)

view :: MenuView
view =
  MenuView {
    range = Just (ViewRange 1 4 4),
    cursor = 2
  }

state :: NvimMenuState
state = NvimMenuState {view, slice = def}

scoreN :: Word -> Word -> Int -> [Word -> (Int, Word, NonEmpty Text)]
scoreN ln s n =
  replicate n (\ index -> (fromIntegral s, index, two))
  where
    two = [exon|line 1 (#{show s})|] :| [[exon|line #{show i}|] | i <- [2..ln]]

score :: Word -> Int -> [Word -> (Int, Word, NonEmpty Text)]
score = scoreN 2

mkEnts :: [[Word -> (Int, Word, NonEmpty Text)]] -> Entries Word
mkEnts scores =
  Entries.multis (zipWith (&) [0..] (mconcat scores))

{-
[1] line 1 (2)
    line 2
[0] line 1 (2)
    line 2
[3] line 1 (4)
    line 2
[2] line 1 (4)
    line 2
[5] line 1 (6)
    line 2
[4] line 1 (6)
    line 2
[7] line 1 (8)
    line 2
[6] line 1 (8)
    line 2
-}
ents :: Entries Word
ents =
  mkEnts [
    score 2 2,
    score 4 2,
    score 6 2,
    score 8 2
  ]

renderEnt :: Entry Word -> [Text]
renderEnt e
  | h :| t <- e.item.render
  , i <- show e.index
  = [exon|[#{i}] #{h}|] : (indent (Text.length i + 3) <$> t)
  where
    indent i t = [exon|#{toText (replicate i ' ')}#{t}|]

renderEntsL :: [Entry Word] -> [Text]
renderEntsL es = renderEnt =<< es

renderEnts :: Entries Word -> Text
renderEnts es =
  Text.unlines ("" : renderEntsL (IntMap.toAscList es >>= reverse . toList . snd))

renderSliceL :: EntrySlice Word -> [Text]
renderSliceL = \case
  EntrySlice {..} ->
    foldMap renderTop partialTop <> renderEntsL full <> foldMap renderBot partialBot
  OnlyPartialEntry {..} ->
    renderBot entry
  where
    renderTop PartialEntry {..} =
      let ls = renderEnt entry
      in split (length ls - fromIntegral visibleLines) ls
    renderBot PartialEntry {..} =
      split (fromIntegral visibleLines) (renderEnt entry)
    split n ls =
      let (before, after) = splitAt n ls
      in before <> ["-------"] <> after

renderSlice :: EntrySlice Word -> Text
renderSlice es = Text.unlines ("" : renderSliceL es)

targetTop1 :: [Text]
targetTop1 =
  [
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2"
  ]

targetTop2 :: [Text]
targetTop2 =
  [
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2",
    "[4] line 1 (6)",
    "-------",
    "    line 2"
  ]

targetTop3 :: [Text]
targetTop3 =
  [
    "[3] line 1 (4)",
    "-------",
    "    line 2"
  ]

sliceTop ::
  HasCallStack =>
  Member (Hedgehog IO) r =>
  [Text] ->
  EntryIndex ->
  AvailLines ->
  Sem r ()
sliceTop target cursor avail =
  withFrozenCallStack do
    assertJust target (renderSliceL <$> entrySliceTop ents cursor avail)

sliceBot ::
  HasCallStack =>
  Member (Hedgehog IO) r =>
  [Text] ->
  EntryIndex ->
  AvailLines ->
  Sem r ()
sliceBot target cursor avail =
  withFrozenCallStack do
    assertJust target (renderSliceL <$> entrySliceBot ents cursor avail)

-- | Slice from cursor index 5 downwards with 6 available lines.
testTop1 :: Member (Hedgehog IO) r => Sem r ()
testTop1 = sliceTop targetTop1 5 6

-- | Slice from cursor index 5 downwards with 7 available lines.
testTop2 :: Member (Hedgehog IO) r => Sem r ()
testTop2 = sliceTop targetTop2 5 7

-- | Slice from cursor index 5 downwards with 1 available line.
testTop3 :: Member (Hedgehog IO) r => Sem r ()
testTop3 = sliceTop targetTop3 5 1

-- | Slice from cursor index 5 downwards with 0 available lines.
testTop4 :: Member (Hedgehog IO) r => Sem r ()
testTop4 = Nothing === entrySliceTop ents 5 0

test_sliceTop :: UnitTest
test_sliceTop =
  runTest do
    testTop1
    testTop2
    testTop3
    testTop4

targetBot1 :: [Text]
targetBot1 =
  [
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2"
  ]

-- | Slice from cursor index 3 upwards with 6 available lines.
testBot1 :: Member (Hedgehog IO) r => Sem r ()
testBot1 = sliceBot targetBot1 3 6

targetBot2 :: [Text]
targetBot2 =
  [
    "[0] line 1 (2)",
    "-------",
    "    line 2",
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2"
  ]

-- | Slice from cursor index 3 upwards with 7 available lines.
testBot2 :: Member (Hedgehog IO) r => Sem r ()
testBot2 = sliceBot targetBot2 3 7

targetBot3 :: [Text]
targetBot3 =
  [
    "[5] line 1 (6)",
    "-------",
    "    line 2"
  ]

-- | Slice from cursor index 3 upwards with 1 available line.
testBot3 :: Member (Hedgehog IO) r => Sem r ()
testBot3 = sliceBot targetBot3 3 1

-- | Slice from cursor index 3 upwards with 0 available lines.
testBot4 :: Member (Hedgehog IO) r => Sem r ()
testBot4 = Nothing === entrySliceBot ents 3 0

targetBot5 :: [Text]
targetBot5 =
  [
    "[1] line 1 (2)",
    "    line 2",
    "[0] line 1 (2)",
    "    line 2"
  ]

-- | Slice from cursor index 6 upwards with 6 available lines.
testBot5 :: Member (Hedgehog IO) r => Sem r ()
testBot5 = sliceBot targetBot5 6 6

test_sliceBot :: UnitTest
test_sliceBot =
  runTest do
    testBot1
    testBot2
    testBot3
    testBot4
    testBot5

cursorSliceWith ::
  Entries Word ->
  CursorIndex ->
  CursorIndex ->
  CursorLine ->
  AvailLines ->
  Maybe ([Text], EntryIndex, EntryIndex, CursorLine)
cursorSliceWith entries oldCursor cursor oldLine avail = do
  slice <- entrySliceCursor entries oldCursor oldLine avail
  let (bottom, top) = sliceRange slice
  pure (renderSliceL slice, bottom, top, findCursorLine cursor slice)

cursorSlice ::
  CursorIndex ->
  CursorIndex ->
  CursorLine ->
  AvailLines ->
  Maybe ([Text], EntryIndex, EntryIndex, CursorLine)
cursorSlice =
  cursorSliceWith ents

targetCursor1 :: [Text]
targetCursor1 =
  [
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2",
    "[4] line 1 (6)",
    "    line 2",
    "[7] line 1 (8)",
    "-------",
    "    line 2"
  ]

-- | Slice moving from cursor index 3 to 2, cursor line 4 and 9 available lines.
-- Before:
--
-- [3] line 1 (4)
--     line 2
-- [2] line 1 (4)
--     line 2
-- [5] line 1 (6) *** Cursor (5th line from the bottom, variable (4) is zero-indexed)
--     line 2         The entry has zero-based index 3 (the bottommost entry is not in the range)
-- [4] line 1 (6)
--     line 2
-- [7] line 1 (8)
--
-- After:
--
-- [3] line 1 (4)
--     line 2
-- [2] line 1 (4)
--     line 2
-- [5] line 1 (6)
--     line 2
-- [4] line 1 (6) *** Cursor moved down one entry index, which is two UI lines
--     line 2
-- [7] line 1 (8)
testCursor1 :: Member (Hedgehog IO) r => Sem r ()
testCursor1 = assertJust (targetCursor1, 2, 5, 2) (cursorSlice 3 2 4 9)

targetCursor2 :: [Text]
targetCursor2 =
  [
    "[5] line 1 (6)",
    "    line 2",
    "[4] line 1 (6)",
    "    line 2",
    "[7] line 1 (8)",
    "    line 2",
    "[6] line 1 (8)",
    "    line 2"
  ]

-- | Slice moving from cursor index 0 to 2, cursor line 5 and 8 available lines.
--
-- This moves the cursor down four lines, because there are no entries below index 0, and the additional space is filled
-- by entries above.
testCursor2 :: Member (Hedgehog IO) r => Sem r ()
testCursor2 = assertJust (targetCursor2, 0, 3, 1) (cursorSlice 2 0 5 8)

targetCursor3 :: [Text]
targetCursor3 =
  [
    "[3] line 1 (4)",
    "-------",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2",
    "[4] line 1 (6)",
    "    line 2",
    "[7] line 1 (8)",
    "-------",
    "    line 2"
  ]

-- | Slice moving from cursor index 2 to 3, with cursor line 2 and 8 available lines.
testCursor3 :: Member (Hedgehog IO) r => Sem r ()
testCursor3 = assertJust (targetCursor3, 2, 4, 4) (cursorSlice 2 3 2 8)

targetCursor4 :: [Text]
targetCursor4 =
  [
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2",
    "[4] line 1 (6)",
    "    line 2"
  ]

-- | Slice moving from cursor index 4 to 5, with cursor line 5 and 8 available lines.
testCursor4 :: Member (Hedgehog IO) r => Sem r ()
testCursor4 = assertJust (targetCursor4, 2, 5, 7) (cursorSlice 4 5 5 8)

targetCursor5 :: [Text]
targetCursor5 =
  [
    "[1] line 1 (2)",
    "    line 2",
    "[0] line 1 (2)",
    "    line 2",
    "[3] line 1 (4)",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2"
  ]

-- | Slice moving from cursor index 7 to 6, with cursor line 7 and 8 available lines.
testCursor5 :: Member (Hedgehog IO) r => Sem r ()
testCursor5 = assertJust (targetCursor5, 4, 7, 5) (cursorSlice 7 6 7 8)

test_sliceCursor :: UnitTest
test_sliceCursor =
  runTest do
    testCursor1
    testCursor2
    testCursor3
    testCursor4
    testCursor5

{-
[1] line 1 (5)
    line 2
    line 3
    line 4
[0] line 1 (5)
    line 2
    line 3
    line 4
-}
entsPartial :: Entries Word
entsPartial =
  mkEnts [
    scoreN 4 5 2
  ]

targetPartial1 :: [Text]
targetPartial1 =
  [
    "[0] line 1 (5)",
    "    line 2",
    "    line 3",
    "    line 4"
  ]

{- | Slice initializing at cursor index 0, with cursor line 2 and 4 available lines.

This will initially produce two 'OnlyPartialEntry' slices:

[1] line 1 (5)
    line 2
    line 3
-------
    line 4
[0] line 1 (5) *** Cursor index and line
    line 2
    line 3
-------
    line 4

And because the lower entry fits entirely into the available four lines, it will be turned into an v'EntrySlice'.
-}
testPartial1 :: Member (Hedgehog IO) r => Sem r ()
testPartial1 = assertJust (targetPartial1, 0, 0, 3) (cursorSliceWith entsPartial 0 0 2 4)

targetPartial2 :: [Text]
targetPartial2 =
  [
    "[0] line 1 (5)",
    "    line 2",
    "    line 3",
    "-------",
    "    line 4"
  ]

-- | Slice initializing at cursor index 0, with cursor line 1 and 3 available lines.
--
-- This will produce only a single partial entry that is moved to the top line.
testPartial2 :: Member (Hedgehog IO) r => Sem r ()
testPartial2 = assertJust (targetPartial2, 0, 0, 2) (cursorSliceWith entsPartial 0 0 1 3)

targetPartial3 :: [Text]
targetPartial3 =
  [
    "[1] line 1 (5)",
    "    line 2",
    "    line 3",
    "-------",
    "    line 4",
    "[0] line 1 (5)",
    "    line 2",
    "    line 3",
    "    line 4"
  ]

-- | Slice initializing at cursor index 0, with cursor line 2 and 5 available lines.
--
-- Same situation as 'testPartial1', but since there is one more line available, entry 1 will be included as a partial.
testPartial3 :: Member (Hedgehog IO) r => Sem r ()
testPartial3 = assertJust (targetPartial3, 0, 0, 3) (cursorSliceWith entsPartial 0 0 2 5)

test_slicePartial :: UnitTest
test_slicePartial =
  runTest do
    testPartial1
    testPartial2
    testPartial3

targetState :: [Text]
targetState =
  [
    "[3] line 1 (4)",
    "-------",
    "    line 2",
    "[2] line 1 (4)",
    "    line 2",
    "[5] line 1 (6)",
    "    line 2",
    "[4] line 1 (6)",
    "    line 2",
    "[7] line 1 (8)",
    "    line 2",
    "[6] line 1 (8)",
    "-------",
    "    line 2"
  ]

test_sliceState :: UnitTest
test_sliceState =
  runTest do
    (newState, result) <- runState state $ updateMenuState ents 3 10
    (slice, changed) <- evalMaybe result
    True === changed
    newView === newState.view
    targetState === renderSliceL slice
  where
    newView =
      MenuView {
        range = Just (ViewRange 1 4 6),
        cursor = 3
      }

test_slice :: TestTree
test_slice =
  testGroup "entry slices" [
    unitTest "cursor at top" test_sliceTop,
    unitTest "cursor at bottom" test_sliceBot,
    unitTest "cursor moved" test_sliceCursor,
    unitTest "only partials" test_slicePartial,
    unitTest "state" test_sliceState
  ]
