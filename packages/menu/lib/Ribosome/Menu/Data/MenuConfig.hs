module Ribosome.Menu.Data.MenuConfig where

data MenuConfig =
  MenuConfig {
    sync :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default MenuConfig where
  def =
    MenuConfig False

menuSync ::
  Member (Reader MenuConfig) r =>
  Sem r a ->
  Sem r a
menuSync =
  local (#sync .~ True)
