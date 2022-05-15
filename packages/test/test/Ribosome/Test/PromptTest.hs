module Ribosome.Test.PromptTest where

import Hedgehog (TestT, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import TestError (TestError)

import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptFlag (StartInsert))
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import Ribosome.Menu.Prompt.Run (processPromptEvent)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (unitTestDef')

promptSetTest :: TestT (Ribo () TestError) ()
promptSetTest = do
  initial <- newMVar initialPrompt
  update <- Stream.last (exec initial (Stream.fromPure event))
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
      ((Prompt 8 PromptState.Normal text), PromptEvent.Edit)

test_promptSet :: UnitTest
test_promptSet =
  unitTestDef' promptSetTest
