module Ribosome.Menu.Data.MenuQuery where

newtype MenuQuery =
  MenuQuery { unMenuQuery :: Text }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Ord, IsString)
