{-# options_ghc -Wno-orphans #-}

module Ribosome.Menu.Data.TestMenuConfig where

import Streamly.Prelude (SerialT)
import Time (NanoSeconds)

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Prompt.Data.Prompt (PromptModes (StartInsert), PromptState)

newtype TestTimeout =
  TestTimeout { unTestTimeout :: NanoSeconds }
  deriving stock (Eq, Show)

instance Show (SerialT IO (MenuItem i)) where
  showsPrec _ _ = showString "<stream>"

data TestMenuConfig i =
  TestMenuConfig {
    items :: Maybe (SerialT IO (MenuItem i)),
    nativePrompt :: Maybe Bool,
    initialItems :: Maybe Bool,
    prompt :: Maybe PromptState,
    addBuiltin :: Maybe Bool,
    addDefault :: Maybe Bool,
    timeout :: Maybe TestTimeout
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)

confDefault :: Lens' (TestMenuConfig i) (Maybe a) -> a -> TestMenuConfig i -> TestMenuConfig i
confDefault attr a =
  attr %~ (<|> Just a)

confSet :: Lens' (TestMenuConfig i) (Maybe a) -> a -> TestMenuConfig i -> TestMenuConfig i
confSet attr a =
  attr .~ Just a

noItems :: TestMenuConfig i -> TestMenuConfig i
noItems = confSet #initialItems False

noItemsConf :: TestMenuConfig i
noItemsConf = noItems def

startInsert :: TestMenuConfig i -> TestMenuConfig i
startInsert =
  confSet #prompt (def & #modes .~ StartInsert)

startInsertConf :: TestMenuConfig i
startInsertConf = startInsert def
