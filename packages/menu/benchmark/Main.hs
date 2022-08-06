module Main where

import Conc (
  Restoration,
  consumeElem,
  interpretAtomic,
  interpretEventsChan,
  interpretGate,
  interpretGates,
  interpretMaskFinal,
  interpretRace,
  subscribeAsync,
  )
import qualified Control.Exception as Base
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, env, whnfIO)
import Exon (exon)
import Lens.Micro.Extras (view)
import Log (Severity (Warn), interpretLogStderrLevelConc)
import Path (relfile)
import Polysemy.Conc.Gate (gate, signal)
import Polysemy.Input (Input (Input))
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, TestError, interpretTestInSubdir)
import Prelude hiding (consume)
import Ribosome.Embed (EmbedStack, runEmbedPluginIO_)
import Ribosome.Host.Interpreter.MState (interpretMState)
import Ribosome.Menu.Combinators (sortedEntries)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Refined))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuConfig (MenuConfig (MenuConfig))
import Ribosome.Menu.Data.IMenuEvent (IMenuEvent (Exhausted, Rendered))
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Effect.MenuState (readMenu)
import Ribosome.Menu.Interpreter.Menu (interpretNvimMenusFinal, runNvimMenu)
import Ribosome.Menu.Interpreter.MenuApp (basic)
import Ribosome.Menu.Interpreter.MenuFilter (interpretMenuFilterFuzzy)
import Ribosome.Menu.Interpreter.MenuState (interpretMenuState, interpretMenuStates)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.MenuUi (interceptMenuUiPromptEvents, interpretMenuUiNvimEcho)
import Ribosome.Menu.Loop (menu, menuStream)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptStyle (OnlyInsert))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
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
  interpretGates $
  interpretEventsChan @MenuEvent $
  interpretEventsChan @IMenuEvent $
  interpretEventsChan @(Maybe Prompt) $
  interpretMenuFilterFuzzy @'True $
  interpretMState def $
  interpretMenuState $
  runReader (MenuConfig items) $
  interpretMenuStream do
    subscribe @IMenuEvent $ subscribe @MenuEvent $ subscribeAsync (menuStream *> publish Rendered) do
      consumeElem Exhausted
      publishPrompt 1 "a"
      consumeElem Refined
      publishPrompt 2 "as"
      consumeElem Refined
      publishPrompt 3 "ase"
      consumeElem Refined
      publishPrompt 4 "ased"
      consumeElem Refined
      publishPrompt 5 "asedo"
      consumeElem Refined
      publish Nothing
      consumeElem Rendered
      len <- length . view sortedEntries <$> readMenu
      if len == 1401
      then unit
      else Base.throw (userError [exon|length is #{show len}|])
  where
    items =
      simpleMenuItem () <$> Stream.fromList files
    publishPrompt i t =
      publish (Just (Prompt i PromptMode.Insert t))

-- time                 1.661 s    (1.482 s .. 1.800 s)
--                      0.999 R²   (0.995 R² .. 1.000 R²)
-- mean                 1.613 s    (1.572 s .. 1.653 s)
-- std dev              47.18 ms   (27.92 ms .. 65.86 ms)
-- variance introduced by outliers: 19% (moderately inflated)
menuBench ::
  [Text] ->
  Sem (EmbedStack ()) ()
menuBench files =
  interpretGate $
  interpretEventsChan $
  interpretNvimMenusFinal $
  interpretMenuStates $
  interpretMenuUiNvimEcho do
    r <- runStop $ runNvimMenu items (PromptConfig OnlyInsert) def $ basic $ interceptMenuUiPromptEvents do
      subscribe @IMenuEvent $ subscribe @MenuEvent $ subscribeAsync (menu *> signal) do
        consumeElem Exhausted
        publishPrompt 1 "a"
        consumeElem Refined
        publishPrompt 2 "as"
        consumeElem Refined
        publishPrompt 3 "ase"
        consumeElem Refined
        publishPrompt 4 "ased"
        consumeElem Refined
        publishPrompt 5 "asedo"
        consumeElem Refined
        publish (PromptEvent.Quit Nothing)
        gate
        len <- length . view sortedEntries <$> readMenu
        if len == 1401
        then unit
        else Base.throw (userError [exon|length is #{show len}|])
    either (Base.throw . userError . show) pure r
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

main :: IO ()
main =
  defaultMainWith conf [
    env fileList \ fs ->
      bgroup "menu" [
        -- bench "prompt updates with 29k items" (whnfIO (runBench (appendBench fs))),
        bench "nvim menu with rendering" (whnfIO (runEmbedPluginIO_ "bench" mempty (menuBench fs)))
      ]
  ]
  where
    conf =
      defaultConfig
