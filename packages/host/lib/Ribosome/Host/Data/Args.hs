-- |Special command parameters governing the aggregation of the entire (rest of the) argument list into one value.
module Ribosome.Host.Data.Args where

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
