module Main where

import Conc (
  Restoration,
  consumeElem,
  interpretEventsChan,
  interpretGate,
  interpretGates,
  interpretMaskFinal,
  interpretQueueTBM,
  interpretRace,
  interpretSync,
  subscribeAsync,
  )
import qualified Control.Exception as Base
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, env, whnfIO)
import Exon (exon)
import Log (Severity (Warn), interpretLogStderrLevelConc)
import Path (relfile)
import Polysemy.Conc.Gate (gate, signal)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, TestError, interpretTestInSubdir)
import Prelude hiding (consume)
import qualified Queue
import Ribosome.Embed (EmbedStack, runEmbedPluginIO_)
import Ribosome.Host.Effect.MState (mread)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpreter.MState (interpretMState)
import Ribosome.Menu.Combinators (sortedEntries)
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted, Query, Rendered), QueryEvent (Refined))
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.RenderEvent (RenderEvent)
import Ribosome.Menu.Data.State (ModalState, modal)
import Ribosome.Menu.Effect.Menu (Menu, bundleMenuEngine, menuEngine)
import Ribosome.Menu.Interpreter.Menu (MS (MS), interpretMenuLoopDeps, interpretMenus, menuStream, unMS)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.MenuUi (interceptMenuUiPromptEvents, interpretMenuUiNvimNull)
import Ribosome.Menu.Items (currentEntries)
import Ribosome.Menu.Loop (addMenuUi, lookupMapping, menuLoop', runMenu)
import Ribosome.Menu.Mappings (defaultMappings)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import qualified Streamly.Prelude as Stream
import System.IO.Error (userError)

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

-- time                 1.388 s    (1.375 s .. 1.408 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 1.415 s    (1.401 s .. 1.426 s)
-- std dev              14.25 ms   (6.179 ms .. 18.78 ms)
-- variance introduced by outliers: 19% (moderately inflated)
appendBench ::
  ∀ r .
  Members [Log, Resource, Race, Mask Restoration, Async, Embed IO, Final IO] r =>
  [Text] ->
  Sem r ()
appendBench files =
  interpretQueueTBM @RenderEvent 64 $
  interpretQueueTBM @(Maybe Prompt) 64 $
  interpretSync $
  interpretGates $
  interpretEventsChan @MenuEvent $
  defaultFilter $
  interpretMState (MS (modal Fuzzy)) $
  interpretMenuStream do
    subscribe @MenuEvent $ subscribeAsync (menuStream @(ModalState ()) items *> publish Rendered) do
      consumeElem Exhausted
      publishPrompt 1 "a"
      consumeElem (Query Refined)
      publishPrompt 2 "as"
      consumeElem (Query Refined)
      publishPrompt 3 "ase"
      consumeElem (Query Refined)
      publishPrompt 4 "ased"
      consumeElem (Query Refined)
      publishPrompt 5 "asedo"
      consumeElem (Query Refined)
      Queue.close
      consumeElem Rendered
      len <- length <$> toListOf (sortedEntries . each . #item . #text) . unMS <$> mread
      if len == 1401
      then unit
      else Base.throw (userError [exon|length is #{show len}|])
  where
    items =
      simpleMenuItem () <$> Stream.fromList files
    publishPrompt i t =
      Queue.write (Just (Prompt i PromptMode.Insert t))

-- time                 1.569 s    (1.357 s .. 1.695 s)
--                      0.998 R²   (0.994 R² .. 1.000 R²)
-- mean                 1.464 s    (1.406 s .. 1.503 s)
-- std dev              58.12 ms   (22.92 ms .. 74.51 ms)
-- variance introduced by outliers: 19% (moderately inflated)
menuBench ::
  [Text] ->
  Sem (EmbedStack ()) ()
menuBench files =
  interpretGate $
  interpretEventsChan @PromptEvent $
  defaultFilter $
  interpretMenuLoopDeps $
  interpretMenuUiNvimNull $
  interpretMenus do
    resumeBootError $ addMenuUi () $ runMenu items (modal Fuzzy) $ bundleMenuEngine $ interceptMenuUiPromptEvents do
      subscribe @MenuEvent $ subscribeAsync (menuLoop' (lookupMapping defaultMappings) *> signal) do
        consumeElem Exhausted
        publishPrompt 1 "a"
        consumeElem (Query Refined)
        publishPrompt 2 "as"
        consumeElem (Query Refined)
        publishPrompt 3 "ase"
        consumeElem (Query Refined)
        publishPrompt 4 "ased"
        consumeElem (Query Refined)
        publishPrompt 5 "asedo"
        consumeElem (Query Refined)
        publish (PromptEvent.Quit Nothing)
        gate
        len <- length <$> menuEngine @(Menu _) currentEntries
        if len == 1401
        then unit
        else Base.throw (userError [exon|length is #{show len}|])
  where
    items =
      simpleMenuItem () <$> Stream.fromList files
    publishPrompt i t =
      publish (PromptEvent.Update (Prompt i PromptMode.Insert t))

runBench :: Sem [Log, Race, Mask Restoration, Async, Resource, Embed IO, Final IO] a -> IO a
runBench =
  runFinal .
  embedToFinal .
  resourceToIOFinal .
  asyncToIOFinal .
  interpretMaskFinal .
  interpretRace .
  interpretLogStderrLevelConc (Just Warn)

menuBenchIO ::
  [Text] ->
  IO ()
menuBenchIO fs =
  runEmbedPluginIO_ "bench" mempty (menuBench fs)

main :: IO ()
main =
  defaultMainWith conf [
    env fileList \ fs ->
      bgroup "menu" [
        bench "prompt updates with 29k items" (whnfIO (runBench (appendBench fs))),
        bench "nvim menu with rendering" (whnfIO (menuBenchIO fs))
      ]
  ]
  where
    conf =
      defaultConfig
