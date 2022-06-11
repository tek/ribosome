module Ribosome.Test.PersistTest where

import Data.Aeson (FromJSON, ToJSON)
import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertJust)

import qualified Ribosome.Effect.Persist as Persist
import Ribosome.Host.Test.Run (embedTest_)
import Ribosome.Interpreter.Persist (interpretPersist)
import Ribosome.Interpreter.PersistPath (interpretPersistPathAt)
import Ribosome.Test.Error (resumeTestError)

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
  embedTest_ do
    dir <- Test.tempDir [reldir|persist|]
    interpretPersistPathAt False dir $ interpretPersist "thing" $ resumeTestError do
      Persist.store Nothing thing
      assertJust thing =<< Persist.load Nothing
