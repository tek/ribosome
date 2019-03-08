{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SettingSpec(
  htf_thisModulesTests,
) where

import Data.Default (def)
import Data.Functor (void)
import Test ()
import Test.Framework

import Ribosome.Config.Setting (setting, updateSetting)
import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, RiboE, riboE2ribo)
import Ribosome.Data.Setting (Setting(Setting), SettingError)
import Ribosome.Test.Unit (unitSpecE, unitSpecR)

sett :: Setting Int
sett = Setting "name" True Nothing

settingSuccessSpec :: RiboE s SettingError (ConcNvimS s) ()
settingSuccessSpec = do
  updateSetting sett 5
  r <- setting sett
  gassertEqual 5 r

test_settingSuccess :: IO ()
test_settingSuccess =
  unitSpecE def () settingSuccessSpec

settingFailSpec :: Ribo s (ConcNvimS s) ()
settingFailSpec = do
  ea <- riboE2ribo result
  void $ gassertLeft ea
  where
    result :: RiboE s SettingError (ConcNvimS s) Int
    result = setting sett

test_settingFail :: IO ()
test_settingFail =
  unitSpecR def () settingFailSpec
