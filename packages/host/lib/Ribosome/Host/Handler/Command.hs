module Ribosome.Host.Handler.Command where

import Type.Errors.Pretty (type (%), type (<>))

import Ribosome.Host.Data.Args (ArgList, Args, JsonArgs)
import Ribosome.Host.Data.Bang (Bang)
import Ribosome.Host.Data.Bar (Bar)
import Ribosome.Host.Data.CommandMods (CommandMods)
import Ribosome.Host.Data.CommandRegister (CommandRegister)
import Ribosome.Host.Data.Range (Range, RangeStyleOpt (rangeStyleArg, rangeStyleOpt))

data ArgCount =
  Zero
  |
  MinZero
  |
  MinOne
  deriving stock (Eq, Show)

type family Max (l :: ArgCount) (r :: ArgCount) :: ArgCount where
  Max 'Zero r = r
  Max 'MinZero 'MinOne = 'MinOne
  Max l _ = l

data OptionState =
  OptionState {
    allowed :: Bool,
    minArgs :: ArgCount,
    optional :: Bool,
    argsConsumed :: Maybe Type
  }

type OptionStateZero =
  'OptionState 'True 'Zero 'True 'Nothing

type family CommandSpecial (a :: Type) :: Bool where
  CommandSpecial (Range _) = 'True
  CommandSpecial Bang = 'True
  CommandSpecial Bar = 'True
  CommandSpecial CommandMods = 'True
  CommandSpecial CommandRegister = 'True
  CommandSpecial Args = 'True
  CommandSpecial ArgList = 'True
  CommandSpecial (JsonArgs _) = 'True
  CommandSpecial _ = 'False

class SpecialParam (state :: OptionState) (a :: Type) where
  type TransSpecial state a :: OptionState
  type TransSpecial s _ =
    s

  specialOpt :: Maybe Text
  specialOpt =
    Nothing

  specialArg :: Maybe Text
  specialArg =
    Nothing

type family BeforeRegular (allowed :: Bool) (a :: Type) :: Constraint where
  BeforeRegular 'False a =
    TypeError ("Command option type " <> a <> " may not come after non-option") ~ ()
  BeforeRegular 'True _ =
    ()

instance (
    BeforeRegular al (Range rs),
    RangeStyleOpt rs
  ) => SpecialParam ('OptionState al c o ac) (Range rs) where
  specialOpt =
    Just (rangeStyleOpt @rs)
  specialArg =
    Just (rangeStyleArg @rs)

instance (
    BeforeRegular al Bang
  ) => SpecialParam ('OptionState al c o ac) Bang where
  specialOpt =
    Just "-bang"
  specialArg =
    Just "'<bang>' == '!'"

instance (
    BeforeRegular al Bar
  ) => SpecialParam ('OptionState al c o ac) Bar where
  specialOpt =
    Just "-bar"
  specialArg =
    Nothing

instance (
    BeforeRegular al CommandMods
  ) => SpecialParam ('OptionState al c o ac) CommandMods where
  specialOpt =
    Nothing
  specialArg =
    Just "<q-mods>"

instance (
    BeforeRegular al CommandRegister
  ) => SpecialParam ('OptionState al c o ac) CommandRegister where
  specialOpt =
    Just "-register"
  specialArg =
    Just "<q-register>"

instance SpecialParam ('OptionState al count o 'Nothing) Args where
  type TransSpecial ('OptionState al count o _) _ =
    'OptionState 'True (Max count 'MinZero) o ('Just Args)

instance SpecialParam ('OptionState al count o ac) ArgList where
  type TransSpecial ('OptionState al count o _) _ =
    'OptionState 'True (Max count 'MinZero) o ('Just ArgList)

class RegularParam (state :: OptionState) (isMaybe :: Bool) a where
  type TransRegular state isMaybe a :: OptionState

type family ArgsError consumer a where
  ArgsError consumer a =
    TypeError (
      "Custom parameter types (here " <> a <> ") cannot be combined with " <> consumer
      %
      "since " <> consumer <> " consumes all arguments"
    )

instance RegularParam ('OptionState al count opt ('Just consumer)) m a where
  type TransRegular ('OptionState al count opt ('Just consumer)) m a =
    ArgsError consumer a

instance RegularParam ('OptionState al count opt 'Nothing) 'True (Maybe a) where
  type TransRegular ('OptionState al count opt 'Nothing) 'True (Maybe a) =
    'OptionState 'False (Max count 'MinZero) opt 'Nothing

instance RegularParam ('OptionState al count opt 'Nothing) 'False a where
  type TransRegular ('OptionState al count opt 'Nothing) 'False a =
    'OptionState 'False 'MinOne 'False 'Nothing

class CommandParam (special :: Bool) (state :: OptionState) (a :: Type) where
  type TransState special state a :: OptionState

  paramOpt :: Maybe Text
  paramOpt =
    Nothing

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

class CommandHandler (state :: OptionState) h where
  commandOptions :: ([Text], [Text])

instance CommandHandler ('OptionState _a 'Zero o c) (Sem r a) where
  commandOptions =
    (["-nargs=0"], [])

instance CommandHandler ('OptionState _a 'MinZero o c) (Sem r a) where
  commandOptions =
    (["-nargs=*"], ["<f-args>"])

instance CommandHandler ('OptionState _a 'MinOne o c) (Sem r a) where
  commandOptions =
    (["-nargs=+"], ["<f-args>"])

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
          maybeToList (paramOpt @special @state @a) <> optsAfter
        args =
          maybeToList (paramArg @special @state @a) <> argsAfter
        (optsAfter, argsAfter) =
          commandOptions @next @b
