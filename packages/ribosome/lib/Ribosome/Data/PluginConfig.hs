module Ribosome.Data.PluginConfig where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.HostConfig (HostConfig)
import Options.Applicative (Parser)
import GHC.Show (showParen)
import Text.Show (showsPrec)
import Exon (exon)

data PluginConfig c =
  PluginConfig {
    name :: PluginName,
    host :: HostConfig,
    custom :: Parser c
  }
  deriving stock (Generic)

instance Show (PluginConfig c) where
  showsPrec d PluginConfig {..} =
    showParen (d > 10) [exon|PluginConfing { name = #{showsPrec 11 name}, host = #{showsPrec 11 host} }|]

instance Eq (PluginConfig c) where
  PluginConfig ln lh _ == PluginConfig rn rh _ =
    ln == rn && lh == rh

pluginNamed :: PluginName -> PluginConfig ()
pluginNamed name =
  PluginConfig name def unit

instance IsString (PluginConfig ()) where
  fromString =
    pluginNamed . fromString
