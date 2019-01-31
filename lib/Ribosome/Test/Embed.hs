module Ribosome.Test.Embed(
  defaultTestConfig,
  defaultTestConfigWith,
  TestConfig (..),
  Vars(..),
  unsafeEmbeddedSpec,
  setVars,
  setupPluginEnv,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(def))
import Data.Foldable (traverse_)
import Neovim (Neovim, Object, vim_set_var')
import Neovim.Test (testWithEmbeddedNeovim, Seconds(..))
import Ribosome.Api.Option (rtpCat)
import Ribosome.Control.Ribo (Ribo)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newInternalTVar)
import System.Directory (makeAbsolute)

type Runner env = TestConfig -> Neovim env () -> Neovim env ()

newtype Vars = Vars [(String, Object)]

data TestConfig =
  TestConfig {
    pluginName :: String,
    extraRtp :: String,
    logPath :: FilePath,
    tcTimeout :: Word,
    variables :: Vars
  }

instance Default TestConfig where
  def = TestConfig "ribosome" "test/f/fixtures/rtp" "test/f/temp/log" 5 (Vars [])

defaultTestConfigWith :: String -> Vars -> TestConfig
defaultTestConfigWith name = TestConfig name "test/f/fixtures/rtp" "test/f/temp/log" 5

defaultTestConfig :: String -> TestConfig
defaultTestConfig name = defaultTestConfigWith name (Vars [])

setVars :: Vars -> Neovim e ()
setVars (Vars vars) =
  traverse_ (uncurry vim_set_var') vars

setupPluginEnv :: TestConfig -> Neovim e ()
setupPluginEnv (TestConfig _ rtp _ _ vars) = do
  absRtp <- liftIO $ makeAbsolute rtp
  rtpCat absRtp
  setVars vars

unsafeEmbeddedSpec :: Runner (Ribosome e) -> TestConfig -> e -> Ribo e () -> IO ()
unsafeEmbeddedSpec runner conf env spec = do
  internal <- newInternalTVar
  testWithEmbeddedNeovim Nothing (Seconds (tcTimeout conf)) (Ribosome (pluginName conf) internal env) $ runner conf spec
