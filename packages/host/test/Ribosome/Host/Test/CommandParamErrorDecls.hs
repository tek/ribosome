{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Ribosome.Host.Test.CommandParamErrorDecls where

import Ribosome.Host.Data.Args (Args)
import Ribosome.Host.Data.Bang (Bang)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Handler.Command (OptionStateZero, commandOptions)

argAfterArgs ::
  ([Text], [Text])
argAfterArgs =
  commandOptions @OptionStateZero @(Args -> Int -> ())

argsAfterArg ::
  ([Text], [Text])
argsAfterArg =
  commandOptions @OptionStateZero @(Int -> Bang -> Sem '[Stop Report] ())
