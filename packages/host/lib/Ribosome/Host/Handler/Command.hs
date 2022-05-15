module Ribosome.Host.Handler.Command where

import Type.Errors.Pretty (type (%), type (<>))

import Ribosome.Host.Data.Args (Args, JsonArgs)
import Ribosome.Host.Data.Bang (Bang)
import Ribosome.Host.Data.Bar (Bar)
import Ribosome.Host.Data.CommandMods (CommandMods)
import Ribosome.Host.Data.CommandRegister (CommandRegister)
import Ribosome.Host.Data.Range (Range, RangeStyleOpt (rangeStyleArg, rangeStyleOpt))

data OptionState =
  OptionState {
    allowed :: Bool,
    count :: Nat,
    optional :: Bool,
    argsConsumed :: Maybe Type
  }

type OptionStateZero =
  'OptionState 'True 0 'False 'Nothing

type family CommandSpecial (a :: Type) :: Bool where
  CommandSpecial (Range _) = 'True
  CommandSpecial Bang = 'True
  CommandSpecial Bar = 'True
  CommandSpecial CommandMods = 'True
  CommandSpecial CommandRegister = 'True
  CommandSpecial Args = 'True
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

instance SpecialParam ('OptionState 'False c o ac) a where
    type TransSpecial ('OptionState 'False c o ac) a =
      TypeError ("Command option type " <> a <> " may not come after non-option")

instance (
    RangeStyleOpt rs
  ) => SpecialParam ('OptionState 'True c o ac) (Range rs) where
  specialOpt =
    Just (rangeStyleOpt @rs)
  specialArg =
    Just (rangeStyleArg @rs)

instance SpecialParam ('OptionState 'True c o ac) Bang where
  specialOpt =
    Just "-bang"
  specialArg =
    Just "'<bang>' == '!'"

instance SpecialParam ('OptionState 'True c o ac) Bar where
  specialOpt =
    Just "-bar"
  specialArg =
    Nothing

instance SpecialParam ('OptionState 'True c o ac) CommandMods where
  specialOpt =
    Nothing
  specialArg =
    Just "<q-mods>"

instance SpecialParam ('OptionState 'True c o ac) CommandRegister where
  specialOpt =
    Just "-register"
  specialArg =
    Just "<q-register>"

instance SpecialParam ('OptionState 'True 0 o ac) Args where
  type TransSpecial ('OptionState 'True 0 o _) _ =
    'OptionState 'True 0 o ('Just Args)

  specialOpt =
    Just "-nargs=1"
  specialArg =
    Just "<f-args>"

class RegularParam (state :: OptionState) (isMaybe :: Bool) a where
  type TransRegular state isMaybe a :: OptionState

  regularOpt :: Maybe Text
  regularOpt =
    Nothing

  regularArg :: Maybe Text
  regularArg =
    Nothing

type family ArgsError consumer a where
  ArgsError consumer a =
    TypeError (
      "Custom parameter types (here " <> a <> ") cannot be combined with " <> consumer
      %
      "since " <> consumer <> " consumes all arguments"
    )

instance RegularParam ('OptionState al 0 opt ('Just consumer)) m a where
  type TransRegular ('OptionState al 0 opt ('Just consumer)) m a =
    ArgsError consumer a

instance RegularParam ('OptionState al 0 opt 'Nothing) 'True (Maybe a) where
  type TransRegular ('OptionState al 0 opt 'Nothing) 'True (Maybe a) =
    'OptionState 'False 1 'True 'Nothing

instance RegularParam ('OptionState al count opt 'Nothing) 'False a where
  type TransRegular ('OptionState al count opt 'Nothing) 'False a =
    'OptionState 'False (count + 1) 'False 'Nothing

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

instance CommandHandler ('OptionState _a c o ('Just consumer)) (Sem r a) where
  commandOptions =
    ([], [])

instance CommandHandler ('OptionState _a c o 'Nothing) (Sem r a) where
  commandOptions =
    (["-nargs=*"], ["<f-args>"])

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
