module Ribosome.Test.PromptTest where

import Conduit (awaitForever, evalStateC, runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as Conduit (last)
import Hedgehog (TestT, (===))
import TestError (TestError)

import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig), PromptFlag(StartInsert))
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed(Yes))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer, processPromptEvent)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (unitSpecDef')

promptSetTest :: TestT (Ribo () TestError) ()
promptSetTest = do
  update <- runConduit $ yield event .| exec .| Conduit.last
  Just target === update
  where
    exec =
      evalStateC initialPrompt (awaitForever (processPromptEvent config))
    initialPrompt =
      Prompt 1 PromptState.Normal "abc"
    config =
      PromptConfig (return ()) basicTransition noPromptRenderer [StartInsert]
    event =
      PromptEvent.Set (Prompt 10 PromptState.Normal text)
    text =
      "12345678"
    target =
      PromptConsumerUpdate event (Prompt 8 PromptState.Normal text) Yes

test_promptSet :: UnitTest
test_promptSet =
  unitSpecDef' promptSetTest
