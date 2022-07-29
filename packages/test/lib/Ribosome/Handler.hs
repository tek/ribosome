module Ribosome.Handler (
  module Ribosome.Host.Data.RpcType,
  module Ribosome.Host.Handler,
) where

import Ribosome.Host.Data.RpcType (
  AutocmdEvent (..),
  AutocmdGroup (..),
  AutocmdOptions (..),
  AutocmdPattern (..),
  CommandCompletion (..),
  CommandOptions (..),
  CompleteStyle (..),
  RpcType (..),
  )
import Ribosome.Host.Handler (
  complete,
  completeBuiltin,
  completeCustom,
  completeWith,
  )
