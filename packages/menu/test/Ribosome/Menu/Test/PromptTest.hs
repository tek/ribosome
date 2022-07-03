module Ribosome.Menu.Test.PromptTest where

import Conc (interpretMaskFinal, interpretSyncAs)
import Log (interpretLogStdoutConc)
import Polysemy.Test (UnitTest, (===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Ribosome.Final (inFinal)
import Ribosome.Host.Test.Run (runTest)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag (StartInsert))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode (PromptMode (..))
import Ribosome.Menu.Prompt.Run (processPromptEvent)

test_promptSet :: UnitTest
test_promptSet =
  runTest $ interpretMaskFinal $ interpretLogStdoutConc $ interpretSyncAs initialPrompt $ interpretPromptEventsDefault [StartInsert] do
    update <- inFinal \ _ lower pur ex -> do
      let
        exec =
          Stream.mapM (fmap join . (fmap ex . lower) . processPromptEvent)
      pur =<< Stream.last (exec (Stream.fromPure event))
    Just (Just target) === update
    where
      initialPrompt =
        Prompt 1 PromptMode.Normal "abc"
      event =
        Left (PromptControlEvent.Set (Prompt 10 PromptMode.Normal text))
      text =
        "12345678"
      target =
        (Prompt 8 PromptMode.Normal text, PromptEvent.Edit)
