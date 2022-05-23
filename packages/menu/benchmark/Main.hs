module Main where

import qualified Control.Exception as Base
import Control.Lens (view)
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, whnfIO)
import Exon (exon)
import Path (relfile)
import Polysemy.Conc (interpretRace, interpretSyncAs)
import Polysemy.Log (Severity (Warn), interpretLogStderrLevelConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, interpretTestInSubdir)
import Ribosome.Final (inFinal)
import Ribosome.Menu.Combinators (sortedEntries)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.MenuStateSem (CursorLock (CursorLock), ItemsLock (ItemsLock), newMenuState, readMenu)
import Ribosome.Menu.Filters (fuzzyItemFilter)
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
    -- (p 1 "a", PromptEvent.Navigation),
    (p 2 "as", PromptEvent.Edit),
    -- (p 1 "a", PromptEvent.Navigation),
    (p 3 "ase", PromptEvent.Edit),
    -- (p 1 "a", PromptEvent.Navigation),
    (p 4 "ased", PromptEvent.Edit),
    -- (p 1 "a", PromptEvent.Navigation),
    (p 5 "asedo", PromptEvent.Edit)
  ]
  where
    p i =
      Prompt i PromptState.Insert

appendBench ::
  ∀ r .
  Members [Test, Log, Resource, Race, Embed IO, Final IO] r =>
  Sem r [MenuEvent]
appendBench = do
  files <- lines <$> Test.fixture [relfile|menu/nixpkgs-files|]
  menuState <- newMenuState
  interpretSyncAs CursorLock $ interpretSyncAs ItemsLock do
    inFinal \ _ lower pur ex -> do
      let
        lowerMaybe :: ∀ x . Sem (Sync ItemsLock : Sync CursorLock : r) x -> IO (Maybe x)
        lowerMaybe =
          fmap ex . lower
        promptStream =
          promptEvent lowerMaybe menuState fuzzyItemFilter events
        itemStream fs =
          Stream.fromSerial (updateItems lowerMaybe menuState fuzzyItemFilter (menuItem <$> Stream.fromList fs))
        menuItem =
          simpleMenuItem ()
      res <- Stream.toList (Stream.async promptStream (itemStream files))
      len <- length . view sortedEntries <$> readMenu menuState
      when (len /= 1401) (Base.throw (userError [exon|length is #{show len}|]))
      pur res

bench_promptAppend :: IO [MenuEvent]
bench_promptAppend = do
  res <- runFinal $
    errorToIOFinal $
    embedToFinal $
    resourceToIOFinal $
    asyncToIOFinal $
    interpretRace $
    interpretLogStderrLevelConc (Just Warn) $
    interpretTestInSubdir "benchmark" do
    appendBench
  either (Base.throw . userError . show) pure res

main :: IO ()
main =
  defaultMainWith conf [
    bgroup "menu" [
      bench "prompt updates with 50k items" (whnfIO bench_promptAppend)
    ]
  ]
  where
    conf =
      defaultConfig
