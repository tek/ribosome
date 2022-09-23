module Ribosome.Test.SettingTest where

import Exon (exon)
import Polysemy.Test (TestError (TestError), UnitTest, assertEq, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Data.Setting (Setting (Setting))
import qualified Ribosome.Data.SettingError as SettingError
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (nvimSetVar)
import Ribosome.Test.Error (resumeTestError)
import Ribosome.Unit.Run (runTestRibosome)

setting :: Setting Int
setting =
  Setting "name" True Nothing

settingDefault :: Setting Int
settingDefault =
  Setting "name" True (Just 13)

test_settingUpdate :: UnitTest
test_settingUpdate =
  runTestRibosome do
    resumeTestError @Settings do
      Settings.update setting 5
      r <- Settings.get setting
      5 === r

test_settingUnset :: UnitTest
test_settingUnset =
  runTestRibosome do
    resumeEither @SettingError (Settings.get setting) >>= \case
      Left (SettingError.Unset _) -> unit
      Right _ -> throw "Unset setting didn't produce error"
      e -> throw (TestError [exon|Invalid setting error for unset: #{show e}|])
    assertEq 13 =<< resumeTestError @Settings (Settings.get settingDefault)

test_settingDecodeError :: UnitTest
test_settingDecodeError =
  runTestRibosome do
    nvimSetVar "test_name" (["text"] :: [Text])
    resumeEither @SettingError (Settings.get setting) >>= \case
      Left (SettingError.Decode _ _) -> unit
      Right _ -> throw "Invalid setting didn't produce error"
      e -> throw (TestError [exon|Invalid setting error for decode error: #{show e}|])

test_settings :: TestTree
test_settings =
  testGroup "settings" [
    unitTest "update" test_settingUpdate,
    unitTest "unset" test_settingUnset,
    unitTest "decode error" test_settingDecodeError
  ]
