module Ribosome.Host.Data.CommandOptions where

import Control.Lens (makeClassy_)

newtype Nargs =
  Nargs { unNargs :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Complete =
  Complete { unComplete :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Addr =
  Addr { unAddr :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Register =
  Register { unRegister :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Range =
  Range { unRange :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

data CommandOptions =
  CommandOptions {
    nargs :: Maybe Nargs,
    complete :: Maybe Complete,
    addr :: Maybe Addr,
    range :: Maybe Range,
    bang :: Bool,
    bar :: Bool,
    register :: Bool,
    buffer :: Bool
  }
  deriving stock (Eq, Show)

makeClassy_ ''CommandOptions

instance Default CommandOptions where
  def =
    CommandOptions Nothing Nothing Nothing Nothing False False False False
