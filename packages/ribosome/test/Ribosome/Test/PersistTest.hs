module Ribosome.Test.PersistTest where

import Data.Aeson (FromJSON, ToJSON)
import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertJust)

import qualified Ribosome.Effect.Persist as Persist
import Ribosome.Host.Embed (embedNvim_)
import Ribosome.Interpreter.Persist (interpretPersist)
import Ribosome.Interpreter.PersistPath (interpretPersistPathAt)
import Ribosome.Test.Error (testError)
import Ribosome.Test.Run (runTest)

data Thing =
  Thing {
    number :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

thing :: Thing
thing =
  Thing 13 "thingo"

test_persist :: UnitTest
test_persist =
  runTest $ embedNvim_ do
    dir <- Test.tempDir [reldir|persist|]
    raiseResumable (interpretPersistPathAt dir) $ interpretPersist "thing" $ testError do
      Persist.store thing
      assertJust thing =<< Persist.load
