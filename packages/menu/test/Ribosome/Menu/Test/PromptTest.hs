module Ribosome.Menu.Test.PromptTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptFlag (StartInsert))
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import Ribosome.Menu.Prompt.Run (processPromptEvent)
import Ribosome.Menu.Prompt.Transition (basicTransition)

test_promptSet :: UnitTest
test_promptSet =
  runTestAuto do
    initial <- embed (newMVar initialPrompt)
    update <- embed (Stream.last (exec initial (Stream.fromPure event)))
    Just (Just target) === update
    where
      exec initial =
        Stream.mapM (processPromptEvent initial (basicTransition [StartInsert]))
      initialPrompt =
        Prompt 1 PromptState.Normal "abc"
      event =
        Left (PromptControlEvent.Set (Prompt 10 PromptState.Normal text))
      text =
        "12345678"
      target =
        (Prompt 8 PromptState.Normal text, PromptEvent.Edit)
