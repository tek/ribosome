module Ribosome.Menu.Test.PromptTest where

import Conc (interpretMaskFinal, interpretSyncAs)
import Log (interpretLogStdoutConc)
import Polysemy.Test (UnitTest, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Ribosome.Final (inFinal)
import Ribosome.Host.Test.Run (runTest)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptFlag (StartInsert))
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import Ribosome.Menu.Prompt.Run (processPromptEvent)
import Ribosome.Menu.Prompt.Transition (basicTransition)

test_promptSet :: UnitTest
test_promptSet =
  runTest $ interpretMaskFinal $ interpretLogStdoutConc $ interpretSyncAs initialPrompt do
    update <- inFinal \ _ lower pur ex -> do
      let
        exec =
          Stream.mapM (fmap join . (fmap ex . lower) . processPromptEvent (basicTransition [StartInsert]))
      pur =<< Stream.last (exec (Stream.fromPure event))
    Just (Just target) === update
    where
      initialPrompt =
        Prompt 1 PromptState.Normal "abc"
      event =
        Left (PromptControlEvent.Set (Prompt 10 PromptState.Normal text))
      text =
        "12345678"
      target =
        (Prompt 8 PromptState.Normal text, PromptEvent.Edit)
