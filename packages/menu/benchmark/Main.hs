module Main where

import qualified Control.Exception as Base
import Control.Lens (view)
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, env, whnfIO)
import Exon (exon)
import Path (relfile)
import Polysemy.Conc (interpretAtomic, interpretRace, interpretSyncAs)
import Polysemy.Log (Severity (Warn), interpretLogStderrLevelConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, TestError, interpretTestInSubdir)
import Ribosome.Final (inFinal)
import Ribosome.Menu.Combinators (sortedEntries)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.MenuState (CursorLock (CursorLock), ItemsLock (ItemsLock), MenuStateSem, readMenu)
import Ribosome.Menu.Filters (fuzzyItemFilterPar)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState
import Ribosome.Menu.UpdateState (promptEvent, updateItems)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream)
import System.IO.Error (userError)

events ::
  MonadIO m =>
  IsStream t =>
  t m (Prompt, PromptEvent)
events =
  Stream.delay 0.000001 $
  Stream.fromList [
    (p 0 "", PromptEvent.Init),
    (p 1 "a", PromptEvent.Edit),
    (p 2 "as", PromptEvent.Edit),
    (p 3 "ase", PromptEvent.Edit),
    (p 4 "ased", PromptEvent.Edit),
    (p 5 "asedo", PromptEvent.Edit)
  ]
  where
    p i =
      Prompt i PromptState.Insert

fatalEither ::
  Show e =>
  Either e a ->
  IO a
fatalEither =
  either (Base.throw . userError . show) pure

runTest :: Sem [Test, Error TestError, Resource, Embed IO, Final IO] a -> IO a
runTest =
  fatalEither <=<
  runFinal .
  embedToFinal .
  resourceToIOFinal .
  errorToIOFinal .
  interpretTestInSubdir "benchmark"

fileList :: IO [Text]
fileList =
  runTest do
    lines <$> Test.fixture [relfile|menu/nixpkgs-files|]

appendBench ::
  ∀ r .
  Members [Log, Resource, Race, Embed IO, Final IO] r =>
  [Text] ->
  Sem r [MenuEvent]
appendBench files = do
  interpretSyncAs CursorLock $
    interpretSyncAs ItemsLock $
    interpretAtomic def $
    interpretAtomic def $
    interpretAtomic def do
      inFinal \ _ lower pur ex -> do
        let
          filt =
            fuzzyItemFilterPar False
          lowerMaybe :: ∀ x . MenuStateSem () r x -> IO (Maybe x)
          lowerMaybe =
            fmap ex . lower
          promptStream =
            promptEvent lowerMaybe filt events
          itemStream =
            Stream.fromSerial (updateItems lowerMaybe filt (menuItem <$> Stream.fromList files))
          menuItem =
            simpleMenuItem ()
        res <- Stream.toList (Stream.async promptStream itemStream)
        len <- lowerMaybe (length . view sortedEntries <$> readMenu)
        when (fromMaybe 0 len /= 1401) (Base.throw (userError [exon|length is #{show len}|]))
        pur res

runBench :: Sem [Log, Race, Async, Resource, Embed IO, Final IO] a -> IO a
runBench =
  runFinal .
  embedToFinal .
  resourceToIOFinal .
  asyncToIOFinal .
  interpretRace .
  interpretLogStderrLevelConc (Just Warn)

main :: IO ()
main =
  defaultMainWith conf [
    env fileList \ fs ->
      bgroup "menu" [
        bench "prompt updates with 29k items" (whnfIO (runBench (appendBench fs)))
      ]
  ]
  where
    conf =
      defaultConfig
