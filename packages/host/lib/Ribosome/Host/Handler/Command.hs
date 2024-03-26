{-# options_haddock prune #-}

-- |Compute the command options and arguments based on handler function parameters.
module Ribosome.Host.Handler.Command where

import Data.MessagePack (Object)

import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Args (ArgList, Args, JsonArgs, Options)
import Ribosome.Host.Data.Bang (Bang)
import Ribosome.Host.Data.Bar (Bar)
import Ribosome.Host.Data.CommandMods (CommandMods)
import Ribosome.Host.Data.CommandRegister (CommandRegister)
import Ribosome.Host.Data.Range (Range, RangeStyleOpt (rangeStyleArg, rangeStyleOpt))

-- |Represents the value for the command option @-nargs@.
data ArgCount =
  -- |@-nargs=0@
  Zero
  |
  -- |@-nargs=*@
  MinZero
  |
  -- |@-nargs=+@
  MinOne
  deriving stock (Eq, Show)

type family Max (l :: ArgCount) (r :: ArgCount) :: ArgCount where
  Max 'Zero r = r
  Max 'MinZero 'MinOne = 'MinOne
  Max l _ = l

-- |Determines how different special command handler parameter types may interact.
data OptionState =
  OptionState {
    -- |Are special option parameters allowed at this position?
    allowed :: Bool,
    -- |The minimum number of arguments that are expected
    minArgs :: ArgCount,
    -- |Have all arguments been consumed, by types like 'ArgList'?
    argsConsumed :: Maybe Type
  }

type OptionStateZero =
  'OptionState 'True 'Zero 'Nothing

type family CommandSpecial (a :: Type) :: Bool where
  CommandSpecial (Range _) = 'True
  CommandSpecial Bang = 'True
  CommandSpecial Bar = 'True
  CommandSpecial CommandMods = 'True
  CommandSpecial CommandRegister = 'True
  CommandSpecial Args = 'True
  CommandSpecial ArgList = 'True
  CommandSpecial (JsonArgs _) = 'True
  CommandSpecial (Options _) = 'True
  CommandSpecial _ = 'False

-- |Determine the command options and arguments that need to be specified when registering a command, for a special
-- command option parameter.
--
-- See [Command params]("Ribosome#g:command-params") for the list of supported special types.
class SpecialParam (state :: OptionState) (a :: Type) where
  type TransSpecial state a :: OptionState
  type TransSpecial s _ =
    s

  specialOpt :: Map Text Object
  specialOpt =
    mempty

  specialArg :: Maybe Text
  specialArg =
    Nothing

-- |Emit a compile error if a special command option type is used as a handler parameter after a regular, value
-- parameter.
--
-- The parameter @allowed@ is set to 'False' when the first non-option parameter is encountered.
type family BeforeRegular (allowed :: Bool) (a :: Type) :: Constraint where
  BeforeRegular 'False a =
    TypeError ("Command option type " <> a <> " may not come after non-option") ~ ()
  BeforeRegular 'True _ =
    ()

instance (
    BeforeRegular al (Range rs),
    RangeStyleOpt rs
  ) => SpecialParam ('OptionState al c ac) (Range rs) where
  specialOpt =
    rangeStyleOpt @rs
  specialArg =
    Just (rangeStyleArg @rs)

instance (
    BeforeRegular al Bang
  ) => SpecialParam ('OptionState al c ac) Bang where
  specialOpt =
    [("bang", toMsgpack True)]
  specialArg =
    Just "'<bang>' == '!'"

instance (
    BeforeRegular al Bar
  ) => SpecialParam ('OptionState al c ac) Bar where
  specialOpt =
    [("bar", toMsgpack True)]
  specialArg =
    Nothing

instance (
    BeforeRegular al CommandMods
  ) => SpecialParam ('OptionState al c ac) CommandMods where
  specialOpt =
    mempty
  specialArg =
    Just "<q-mods>"

instance (
    BeforeRegular al CommandRegister
  ) => SpecialParam ('OptionState al c ac) CommandRegister where
  specialOpt =
    [("register", toMsgpack True)]
  specialArg =
    Just "<q-register>"

instance SpecialParam ('OptionState al count 'Nothing) Args where
  type TransSpecial ('OptionState _ count _) _ =
    'OptionState 'True (Max count 'MinZero) ('Just Args)

instance SpecialParam ('OptionState al count ac) (JsonArgs a) where
  type TransSpecial ('OptionState _ count _) (JsonArgs a) =
    'OptionState 'True (Max count 'MinZero) ('Just (JsonArgs a))

instance SpecialParam ('OptionState al count ac) ArgList where
  type TransSpecial ('OptionState _ count _) _ =
    'OptionState 'True (Max count 'MinZero) ('Just ArgList)

instance SpecialParam ('OptionState al count 'Nothing) (Options a) where
  type TransSpecial ('OptionState _ count _) (Options a) =
    'OptionState 'True (Max count 'MinZero) ('Just (Options a))

-- |Determines whether a regular, value parameter is allowed (it isn't after types like 'ArgList' that consume all
-- remaining arguments), and increases the minimum argument count if the parameter isn't 'Maybe'.
class RegularParam (state :: OptionState) (isMaybe :: Bool) a where
  type TransRegular state isMaybe a :: OptionState

type family ArgsError consumer a where
  ArgsError consumer a =
    TypeError (
      "Custom parameter types (here " <> a <> ") cannot be combined with " <> consumer
      %
      "since " <> consumer <> " consumes all arguments"
    )

instance RegularParam ('OptionState al count ('Just consumer)) m a where
  type TransRegular ('OptionState al count ('Just consumer)) m a =
    ArgsError consumer a

instance RegularParam ('OptionState al count 'Nothing) 'True (Maybe a) where
  type TransRegular ('OptionState al count 'Nothing) 'True (Maybe a) =
    'OptionState 'False (Max count 'MinZero) 'Nothing

instance RegularParam ('OptionState al count 'Nothing) 'False a where
  type TransRegular ('OptionState al count 'Nothing) 'False a =
    'OptionState 'False 'MinOne 'Nothing

-- |Determine the command option and parameter that a handler parameter type requires, if any.
class CommandParam (special :: Bool) (state :: OptionState) (a :: Type) where
  -- |Transition the current 'OptionState'.
  type TransState special state a :: OptionState

  paramOpt :: Map Text Object
  paramOpt =
    mempty

  paramArg :: Maybe Text
  paramArg =
    Nothing

instance (
    SpecialParam state a
  ) => CommandParam 'True state a where
    type TransState 'True state a =
      TransSpecial state a

    paramOpt =
      specialOpt @state @a

    paramArg =
      specialArg @state @a

type family IsMaybe (a :: Type) :: Bool where
  IsMaybe (Maybe _) = 'True
  IsMaybe _ = 'False

instance (
    RegularParam state (IsMaybe a) a
  ) => CommandParam 'False state a where
    type TransState 'False state a =
      TransRegular state (IsMaybe a) a

-- |Derive the command options and arguments that should be used when registering the Neovim command, from the
-- parameters of the handler function.
--
-- See [Command params]("Ribosome#g:command-params") for the list of supported special types.
--
-- The parameter @state@ is a type level value that determines which parameter types may be used after another and
-- counts the number of command arguments that are required or allowed.
-- It is transitioned by families in the classes 'CommandParam', 'SpecialParam' and 'RegularParam'.
class CommandHandler (state :: OptionState) (h :: Type) where
  -- |Return the list of command options and special arguments determined by the handler function's parameters.
  commandOptions :: (Map Text Object, [Text])

instance CommandHandler ('OptionState _a 'Zero c) (Sem r a) where
  commandOptions =
    ([("nargs", toMsgpack @Int 0)], [])

instance CommandHandler ('OptionState _a 'MinZero c) (Sem r a) where
  commandOptions =
    ([("nargs", toMsgpack @Text "*")], ["<f-args>"])

instance CommandHandler ('OptionState _a 'MinOne c) (Sem r a) where
  commandOptions =
    ([("nargs", toMsgpack @Text "+")], ["<f-args>"])

instance (
    special ~ CommandSpecial a,
    next ~ TransState special state a,
    CommandParam special state a,
    CommandHandler next b
  ) => CommandHandler state (a -> b) where
    commandOptions =
      (opts, args)
      where
        opts =
          optsAfter <> paramOpt @special @state @a
        args =
          maybeToList (paramArg @special @state @a) <> argsAfter
        (optsAfter, argsAfter) =
          commandOptions @next @b
