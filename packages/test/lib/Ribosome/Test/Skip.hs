module Ribosome.Test.Skip where

import qualified Data.Text.IO as Text
import Polysemy.Test (UnitTest)
import System.Environment (lookupEnv)

skipUnlessX :: UnitTest -> UnitTest
skipUnlessX test = do
  liftIO (lookupEnv "DISPLAY") >>= \case
    Just _ -> test
    Nothing -> liftIO (Text.putStrLn "Skipping test due to lack of display")
