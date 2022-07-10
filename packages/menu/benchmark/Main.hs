module Main where

import Conc (Restoration, interpretAtomic, interpretMaskFinal, interpretQueueTBM, interpretRace)
import qualified Control.Exception as Base
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, env, whnfIO)
import Exon (exon)
import Lens.Micro.Extras (view)
import Path (relfile)
import Polysemy.Input (Input (Input))
import Polysemy.Log (Severity (Warn), interpretLogStderrLevelConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, TestError, interpretTestInSubdir)
import Prelude hiding (consume)
import qualified Queue
import Ribosome.Final (inFinal_)
import Ribosome.Menu.Combinators (sortedEntries)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted, PromptEdit))
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Data.MenuState (semState)
import Ribosome.Menu.Effect.MenuState (readMenu, useItems)
import qualified Ribosome.Menu.Effect.MenuStream as MenuStream
import Ribosome.Menu.Effect.PromptControl (withPromptControl)
import Ribosome.Menu.Interpreter.MenuFilter (interpretMenuFilterFuzzy)
import Ribosome.Menu.Interpreter.MenuState (interpretMenuState)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Main (sendQuit)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Stream.Util (takeUntilNothing)
import Ribosome.Menu.UpdateState (insertItems, queryUpdate, setPromptAndClassify)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream)
import System.IO.Error (userError)

events :: [(Prompt, PromptEvent)]
events =
  [
    (p 0 "", PromptEvent.Edit),
    (p 1 "a", PromptEvent.Edit),
    (p 2 "as", PromptEvent.Edit),
    (p 3 "ase", PromptEvent.Edit),
    (p 4 "ased", PromptEvent.Edit),
    (p 5 "asedo", PromptEvent.Edit)
  ]
  where
    p i =
      Prompt i PromptMode.Insert

eventStream ::
  MonadIO m =>
  IsStream t =>
  t m (Prompt, PromptEvent)
eventStream =
  Stream.delay 0.000001 $
  Stream.fromList events

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

outAct ::
  MenuAction a ->
  Sem r (Maybe (MenuResult a))
outAct = \case
  MenuAction.Continue ->
    pure Nothing
  MenuAction.Render ->
    pure Nothing
  MenuAction.UpdatePrompt _ ->
    pure Nothing
  MenuAction.Quit result ->
    pure (Just result)

runInputListAtomic ::
  Member (Embed IO) r =>
  [a] ->
  InterpreterFor (Input (Maybe a)) r
runInputListAtomic as =
  interpretAtomic as .
  reinterpret \ Input ->
    atomicState' \case
      [] -> ([], Nothing)
      h : t -> (t, Just h)

-- XXX previous results:
-- benchmarking menu/prompt updates with 29k items
-- time                 1.691 s    (1.470 s .. 1.866 s)
--                      0.998 R²   (0.993 R² .. 1.000 R²)
-- mean                 1.752 s    (1.702 s .. 1.783 s)
-- std dev              41.22 ms   (19.60 ms .. 51.90 ms)
-- variance introduced by outliers: 19% (moderately inflated)
appendBench ::
  ∀ r .
  Members [Log, Resource, Race, Mask Restoration, Embed IO, Final IO] r =>
  [Text] ->
  Sem r (Maybe (MenuResult ()))
appendBench files =
  runInputListAtomic events $
  interpretQueueTBM 64 $
  interpretPromptControl $
  withPromptControl $
  interpretMenuFilterFuzzy @'True $
  interpretMenuState $
  interpretMenuStream do
    pe <- promptEvents
    result <- MenuStream.menuStream items pe (const unit) setPromptAndClassify queryUpdate insert sendQuit consume outAct
    len <- length . view sortedEntries <$> readMenu
    if len == 1401
    then pure result
    else Base.throw (userError [exon|length is #{show len}|])
  where
    items =
      simpleMenuItem () <$> Stream.fromList files
    promptEvents =
      inFinal_ \ lowerMaybe _ pur ->
        pur $ takeUntilNothing $ Stream.repeatM (join <$> lowerMaybe Queue.readMaybe)
    insert new =
      MenuEvent.NewItems <$ useItems \ its -> runState its (semState (insertItems new))
    consume = \case
      Exhausted ->
        takePrompt
      PromptEdit ->
        takePrompt
      _ ->
        pure MenuAction.Continue
    takePrompt =
      input >>= \case
        Just pp ->
          MenuAction.Continue <$ Queue.write pp
        Nothing ->
          MenuAction.Quit MenuResult.NoAction <$ Queue.close

runBench :: Sem [Log, Race, Mask Restoration, Async, Resource, Embed IO, Final IO] a -> IO a
runBench =
  runFinal .
  embedToFinal .
  resourceToIOFinal .
  asyncToIOFinal .
  interpretMaskFinal .
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
