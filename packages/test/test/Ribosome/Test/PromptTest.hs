module Ribosome.Test.PromptTest where

import Hedgehog (TestT, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import TestError (TestError)

import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag (StartInsert))
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed (Yes))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate (PromptConsumerUpdate))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent (..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer, processPromptEvent)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (unitTestDef')

promptSetTest :: TestT (Ribo () TestError) ()
promptSetTest = do
  update <- Streamly.last (exec (Streamly.fromPure event))
  Just target === update
  where
    exec =
      Streamly.evalStateT (pure initialPrompt) . Streamly.mapM (processPromptEvent config)
    initialPrompt =
      Prompt 1 PromptState.Normal "abc" def
    config =
      PromptConfig Streamly.nil basicTransition noPromptRenderer [StartInsert]
    event =
      PromptEvent.Set (Prompt 10 PromptState.Normal text def)
    text =
      "12345678"
    target =
      PromptConsumerUpdate event (Prompt 8 PromptState.Normal text def) Yes

test_promptSet :: UnitTest
test_promptSet =
  unitTestDef' promptSetTest
