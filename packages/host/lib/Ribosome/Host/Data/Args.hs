-- |Special command parameters governing the aggregation of the entire (rest of the) argument list into one value.
module Ribosome.Host.Data.Args where

import Options.Applicative (Parser)

-- |When this type is used as the (last) parameter of a command handler function, all remaining tokens passed to the
-- command will be consumed and stored in this type.
--
-- The command will be declared with the @-nargs=*@ or @-nargs=+@ option.
--
-- See 'Ribosome.CommandHandler'.
newtype Args =
  Args { unArgs :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

-- |When this type is used as the (last) parameter of a command handler function, all remaining tokens passed to the
-- command will be consumed and stored in this type, as a list of whitespace separated tokens.
--
-- The command will be declared with the @-nargs=*@ or @-nargs=+@ option.
--
-- See 'Ribosome.CommandHandler'.
newtype ArgList =
  ArgList { unArgList :: [Text] }
  deriving stock (Eq, Show)

-- |When this type is used as the (last) parameter of a command handler function, all remaining tokens passed to the
-- command will be consumed, decoded as JSON and stored in this type.
--
-- The command will be declared with the @-nargs=*@ or @-nargs=+@ option.
--
-- See 'Ribosome.CommandHandler'.
newtype JsonArgs a =
  JsonArgs { unJsonArgs :: a }
  deriving stock (Eq, Show)

-- |When this type is used as the (last) parameter of a command handler function, all remaining tokens passed to the
-- command will be consumed, parsed via [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
-- and stored in this type.
--
-- The parser associated with @a@ must be defined as an instance of @'OptionParser' a@.
--
-- The command will be declared with the @-nargs=*@ or @-nargs=+@ option.
--
-- See 'Ribosome.CommandHandler'.
newtype Options a =
  Options a
  deriving stock (Eq, Show)

-- |The parser used when declaring command handlers with the special parameter @'Options' a@.
class OptionParser a where
  optionParser :: Parser a
