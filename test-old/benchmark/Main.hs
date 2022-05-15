module Main where

import Control.Lens (view)
import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, whnfIO)
import Ribosome.Menu.Data.Menu (sortedEntries)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState
import Ribosome.Menu.UpdateState (promptEvent, updateItems)
import Ribosome.Test.File (fixtureContent)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream)
import Control.Exception (throw)
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
    p i t =
      Prompt i PromptState.Insert t

bench_promptAppend :: IO [MenuEvent]
bench_promptAppend = do
  menu <- newMVar def
  files <- lines <$> fixtureContent "benchmark" "menu/nixpkgs-files"
  res <- Stream.toList (Stream.async (promptStream menu) (itemStream menu files))
  len <- length . view sortedEntries <$> readMVar menu
  when (len /= 1) (throw (userError [exon|length is #{show len}|]))
  pure res
  where
    promptStream menu =
      promptEvent menu fuzzyItemFilter events
    itemStream menu files =
      Stream.fromSerial (updateItems menu fuzzyItemFilter (menuItem <$> Stream.fromList (take 100 files)))
    menuItem =
      simpleMenuItem ()

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
