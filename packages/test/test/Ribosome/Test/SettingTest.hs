module Ribosome.Test.SettingTest where

import Data.Either.Combinators (swapEither)
import Hedgehog (TestT, evalEither, (===))

import Ribosome.Config.Setting (setting, updateSetting)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError (..))
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (unitTest)

data SettingTestError =
  Sett SettingError
  |
  Rpc RpcError
  deriving stock (Show)

instance ReportError SettingTestError where
  errorReport = undefined

deepPrisms ''SettingTestError

sett :: Setting Int
sett = Setting "name" True Nothing

settingSuccessTest :: TestT (Ribo s SettingTestError) ()
settingSuccessTest = do
  updateSetting sett 5
  r <- lift (setting sett)
  5 === r

test_settingSuccess :: UnitTest
test_settingSuccess =
  unitTest def () settingSuccessTest

settingFailTest :: TestT (Ribo s SettingTestError) ()
settingFailTest = do
  ea <- lift (catchAt catch $ Right <$> result)
  void $ evalEither (swapEither ea)
  where
    result :: Ribo s SettingTestError Int
    result = setting sett
    catch :: SettingError -> Ribo s SettingTestError (Either SettingError Int)
    catch = return . Left

test_settingFail :: UnitTest
test_settingFail =
  unitTest def () settingFailTest
