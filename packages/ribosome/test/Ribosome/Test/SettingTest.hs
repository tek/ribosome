module Ribosome.Test.SettingTest where

import Polysemy.Test (UnitTest, assertEq, assertLeft, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Test.Error (testError)
import Ribosome.Test.Run (embedPluginTest_)

setting :: Setting Int
setting =
  Setting "name" True Nothing

settingDefault :: Setting Int
settingDefault =
  Setting "name" True (Just 13)

test_settingUpdate :: UnitTest
test_settingUpdate =
  embedPluginTest_ do
    testError (Settings.update setting 5)
    r <- testError (Settings.get setting)
    5 === r

test_settingUnset :: UnitTest
test_settingUnset =
  embedPluginTest_ do
    ea <- resumeEither @SettingError (Settings.get setting)
    assertLeft () (first unit ea)
    assertEq 13 =<< testError (Settings.get settingDefault)

test_settings :: TestTree
test_settings =
  testGroup "settings" [
    unitTest "update" test_settingUpdate,
    unitTest "unset" test_settingUnset
  ]
