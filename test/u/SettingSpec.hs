{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module SettingSpec(
  htf_thisModulesTests,
) where

import Test.Framework

import Ribosome.Config.Setting (setting, updateSetting)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Test.Unit (unitSpec)

data SettingSpecError =
  Sett SettingError
  |
  Rpc RpcError
  deriving Show

instance ReportError SettingSpecError where
  errorReport = undefined

deepPrisms ''SettingSpecError

sett :: Setting Int
sett = Setting "name" True Nothing

settingSuccessSpec :: Ribo s SettingSpecError ()
settingSuccessSpec = do
  updateSetting sett 5
  r <- setting sett
  gassertEqual 5 r

test_settingSuccess :: IO ()
test_settingSuccess =
  unitSpec def () settingSuccessSpec

settingFailSpec :: Ribo s SettingSpecError ()
settingFailSpec = do
  ea <- catchAt catch $ Right <$> result
  void $ gassertLeft ea
  where
    result :: Ribo s SettingSpecError Int
    result = setting sett
    catch :: SettingError -> Ribo s SettingSpecError (Either SettingError Int)
    catch = return . Left

test_settingFail :: IO ()
test_settingFail =
  unitSpec def () settingFailSpec
