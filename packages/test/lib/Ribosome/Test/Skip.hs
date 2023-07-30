module Ribosome.Test.Skip where

import qualified Data.Text.IO as Text
import Polysemy.Test (UnitTest)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, withResource)

displayPresent :: IO Bool
displayPresent = isJust <$> lookupEnv "DISPLAY"

displayMessage :: IO ()
displayMessage = Text.putStrLn "Skipping test due to lack of display"

skipUnlessX :: UnitTest -> UnitTest
skipUnlessX test = do
  liftIO displayPresent >>= \case
    True -> test
    False -> liftIO displayMessage

displayPresentVerbose :: IO Bool
displayPresentVerbose =
  displayPresent >>= tap \case
    True -> unit
    False -> displayMessage

requireX ::
  (UnitTest -> TestTree) ->
  UnitTest ->
  TestTree
requireX mkTest test =
  withResource displayPresentVerbose (const unit) \ res ->
    mkTest do
      liftIO res >>= \case
        True -> test
        False -> unit
